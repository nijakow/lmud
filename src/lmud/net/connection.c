/**
 * @file connection.c
 * @brief Connections
 * 
 * This file contains the connection structures for the LMud project. 
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#include <lmud/glue.h>
#include <lmud/net/net.h>
#include <lmud/lisp/lisp.h>
#include <lmud/util/memory.h>

#include "connection.h"


void LMud_ConnectionRef_Create(struct LMud_ConnectionRef* self, struct LMud_Connection* connection)
{
    self->connection = connection;
    self->prev       = NULL;
    self->next       = NULL;

    LMud_ConnectionRef_Link(self, &connection->refs);
}

void LMud_ConnectionRef_Destroy(struct LMud_ConnectionRef* self)
{
    LMud_ConnectionRef_Unlink(self);
}

void LMud_ConnectionRef_Link(struct LMud_ConnectionRef* self, struct LMud_ConnectionRef** list)
{
    LMud_ConnectionRef_Unlink(self);

    self->prev =  list;
    self->next = *list;
    if (*list != NULL)
        (*list)->prev = &self->next;
    *list = self;
}

void LMud_ConnectionRef_Unlink(struct LMud_ConnectionRef* self)
{
    if (self->prev != NULL)
        *self->prev = self->next;
    if (self->next != NULL)
        self->next->prev = self->prev;
    self->prev = NULL;
    self->next = NULL;
}

void LMud_ConnectionRef_Kill(struct LMud_ConnectionRef* self)
{
    self->connection = NULL;
    LMud_ConnectionRef_Unlink(self);
}


static void LMud_Connection_ReleaseFibersWithEof(struct LMud_Connection* self);

static void LMud_Connection_KillRefs(struct LMud_Connection* self)
{
    while (self->refs != NULL)
    {
        LMud_ConnectionRef_Kill(self->refs);
    }
}

void LMud_Connection_Create(struct LMud_Connection* self, struct LMud_Net* net, int fd)
{
    self->net  = net;

    self->fd      = fd;
    self->eof     = false;
    self->closing = false;

    self->prev = NULL;
    self->next = NULL;
    self->refs = NULL;

    LMud_FiberQueue_Create(&self->waiting_fibers);
    LMud_FiberQueue_Create(&self->waiting_fibers_eof);

    LMud_Ringbuffer_Create(&self->inbuf,  4096);
    LMud_Ringbuffer_Create(&self->outbuf, 4096);
}

void LMud_Connection_Destroy(struct LMud_Connection* self)
{
    LMud_Connection_CloseImmediately(self);
    LMud_Connection_Unlink(self);
    LMud_Connection_KillRefs(self);
    LMud_FiberQueue_Destroy(&self->waiting_fibers); // TODO, FIXME, XXX: What shall we do with the drunken fiber? ;-)
    LMud_FiberQueue_Destroy(&self->waiting_fibers_eof);
    LMud_Ringbuffer_Destroy(&self->inbuf);
    LMud_Ringbuffer_Destroy(&self->outbuf);
}

void LMud_Connection_Link(struct LMud_Connection* self, struct LMud_Connection** list)
{
    LMud_Connection_Unlink(self);

    self->prev =  list;
    self->next = *list;
    if (*list != NULL)
        (*list)->prev = &self->next;
    *list = self;
}

void LMud_Connection_Unlink(struct LMud_Connection* self)
{
    if (self->prev != NULL)
        *self->prev = self->next;
    if (self->next != NULL)
        self->next->prev = self->prev;
    self->prev = NULL;
    self->next = NULL;
}

bool LMud_Connection_Eof(struct LMud_Connection* self)
{
    return self->eof;
}

bool LMud_Connection_IsClosed(struct LMud_Connection* self)
{
    return self->fd < 0;
}

void LMud_Connection_CloseImmediately(struct LMud_Connection* self)
{
    LMud_Debugf(self->net->mud, LMud_LogLevel_FULL_DEBUG, "FD(%d): Closing connection!", self->fd);
    if (self->fd >= 0)
    {
        close(self->fd);
        self->fd = -1;
    }
    self->eof = true;
    LMud_Connection_ReleaseFibersWithEof(self);
}

void LMud_Connection_RequestClose(struct LMud_Connection* self)
{
    LMud_Debugf(self->net->mud, LMud_LogLevel_FULL_DEBUG, "FD(%d): Requesting connection close!", self->fd);
    self->closing = true;
}


void LMud_Connection_RegisterOnSelector(struct LMud_Connection* self, struct LMud_Selector* selector)
{
    if (!LMud_Connection_Eof(self))
    {
        if (LMud_Ringbuffer_HasData(&self->outbuf))
            LMud_Selector_AddWrite(selector, self->fd);

        if (!self->closing && !LMud_Ringbuffer_IsFull(&self->inbuf))
            LMud_Selector_AddRead(selector, self->fd);
        
        LMud_Selector_AddExcept(selector, self->fd);
    }
}


static void LMud_Connection_ReleaseFibersWithByte(struct LMud_Connection* self, char byte)
{
    while (self->waiting_fibers.fibers != NULL)
    {
        LMud_Debugf(self->net->mud, LMud_LogLevel_FULL_DEBUG, "FD(%d): Fiber %p released from the waiting fiber list!", self->fd);
        LMud_Fiber_ControlRestartWithValue(self->waiting_fibers.fibers, LMud_Any_FromInteger(((LMud_Integer) 0) | (unsigned char) byte));
    }
}

static void LMud_Connection_ReleaseEofFibers(struct LMud_Connection* self, bool eof)
{
    while (self->waiting_fibers_eof.fibers != NULL)
    {
        LMud_Debugf(self->net->mud, LMud_LogLevel_FULL_DEBUG, "FD(%d): Fiber %p released from the waiting EOF fiber list (signalling no EOF)!", self->fd);
        LMud_Fiber_ControlRestartWithValue(self->waiting_fibers_eof.fibers, LMud_Lisp_Boolean(LMud_GetLisp(self->net->mud), eof));
    }
}

static void LMud_Connection_MaybeReleaseFibers(struct LMud_Connection* self)
{
    char  byte;

    LMud_Debugf(self->net->mud, LMud_LogLevel_FULL_DEBUG, "FD(%d): Releasing fibers...", self->fd);

    if (LMud_FiberQueue_HasFibers(&self->waiting_fibers))
    {
        if (LMud_Ringbuffer_ReadByte(&self->inbuf, &byte))
        {
            LMud_Connection_ReleaseFibersWithByte(self, byte);
        }
    }

    if (LMud_FiberQueue_HasFibers(&self->waiting_fibers_eof))
    {
        LMud_Connection_ReleaseEofFibers(self, self->eof);
    }
}

static void LMud_Connection_ReleaseFibersWithEof(struct LMud_Connection* self)
{
    LMud_Debugf(self->net->mud, LMud_LogLevel_FULL_DEBUG, "FD(%d): Releasing fibers with EOF...", self->fd);

    while (self->waiting_fibers.fibers != NULL)
    {
        LMud_Debugf(self->net->mud, LMud_LogLevel_FULL_DEBUG, "FD(%d): Fiber %p released from the waiting fiber list!", self->fd);
        LMud_Fiber_ControlRestartWithValue(self->waiting_fibers.fibers, LMud_Lisp_Nil(LMud_GetLisp(self->net->mud)));
    }

    /*
     * TODO, FIXME: Use `self->eof` here? This would be a bit more logical,
     * but then self->eof must be set before this function is called...
     */
    LMud_Connection_ReleaseEofFibers(self, true);
}

void LMud_Connection_AddWaitingFiber(struct LMud_Connection* self, struct LMud_Fiber* fiber)
{
    LMud_Debugf(self->net->mud, LMud_LogLevel_FULL_DEBUG, "FD(%d): Fiber %p queued into the waiting fibers list!", self->fd);
    LMud_Fiber_ControlWaitOnQueue(fiber, &self->waiting_fibers);
}

void LMud_Connection_AddWaitingFiberEof(struct LMud_Connection* self, struct LMud_Fiber* fiber)
{
    LMud_Debugf(self->net->mud, LMud_LogLevel_FULL_DEBUG, "FD(%d): Fiber %p queued into the waiting EOF fibers list!", self->fd);
    LMud_Fiber_ControlWaitOnQueue(fiber, &self->waiting_fibers_eof);
}

void LMud_Connection_FiberReadByte(struct LMud_Connection* self, struct LMud_Fiber* fiber)
{
    char  byte;

    if (LMud_Ringbuffer_ReadByte(&self->inbuf, &byte)) {
        LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromInteger(((LMud_Integer) 0) | (unsigned char) byte));
    } else if (self->eof) {
        LMud_Fiber_ControlRestartWithValue(fiber, LMud_Lisp_Nil(LMud_GetLisp(self->net->mud)));
    } else {
        LMud_Connection_AddWaitingFiber(self, fiber);
    }
}

void LMud_Connection_FiberEof(struct LMud_Connection* self, struct LMud_Fiber* fiber)
{
    if (LMud_Ringbuffer_HasData(&self->inbuf)) {
        LMud_Fiber_ControlRestartWithValue(fiber, LMud_Lisp_Nil(LMud_GetLisp(self->net->mud)));
    } else if (self->eof) {
        LMud_Fiber_ControlRestartWithValue(fiber, LMud_Lisp_T(LMud_GetLisp(self->net->mud)));
    } else {
        LMud_Connection_AddWaitingFiberEof(self, fiber);
    }
}

bool LMud_Connection_WriteByte(struct LMud_Connection* self, char byte)
{
    return LMud_Ringbuffer_WriteByte(&self->outbuf, byte);
}


void LMud_Connection_HandleError(struct LMud_Connection* self)
{
    LMud_Debugf(self->net->mud, LMud_LogLevel_HALF_DEBUG, "FD(%d): Handling connection error!", self->fd);
    self->eof = true;
    LMud_Connection_ReleaseFibersWithEof(self);
}

void LMud_Connection_HandleDisconnect(struct LMud_Connection* self)
{
    LMud_Debugf(self->net->mud, LMud_LogLevel_HALF_DEBUG, "FD(%d): Handling disconnect!", self->fd);
    LMud_Connection_HandleError(self);
}

void LMud_Connection_Tick(struct LMud_Connection* self, struct LMud_Selector* selector)
{
    size_t   size;
    ssize_t  ssize;
    ssize_t  written;
    char     buffer[1024];

    if (LMud_Connection_IsClosed(self))
        return;

    if (LMud_Selector_IsRead(selector, self->fd))
    {
        size = LMud_Ringbuffer_GetFreeSpace(&self->inbuf);

        LMud_Debugf(self->net->mud, LMud_LogLevel_HALF_DEBUG, "FD(%d): Free space in inbuf: %lu", self->fd, size);

        if (size > sizeof(buffer))
            size = sizeof(buffer);

        ssize = read(self->fd, buffer, size);

        LMud_Debugf(self->net->mud, LMud_LogLevel_HALF_DEBUG, "FD(%d): Read  %4ld bytes", self->fd, ssize);

        if (ssize <= 0) {
            LMud_Connection_HandleDisconnect(self);
        } else {
            LMud_Ringbuffer_WriteBytes(&self->inbuf, buffer, ssize);
            LMud_Connection_MaybeReleaseFibers(self);
        }
    }

    if (LMud_Selector_IsWrite(selector, self->fd))
    {
        written = 0;
        size    = LMud_Ringbuffer_PeekBytes(&self->outbuf, buffer, sizeof(buffer));

        if (size > 0)
        {
            written = write(self->fd, buffer, size);

            LMud_Debugf(self->net->mud, LMud_LogLevel_HALF_DEBUG, "FD(%d): Wrote %4ld bytes out of %4lu", self->fd, written, size);

            if (written < 0) {
                LMud_Connection_HandleDisconnect(self);
            } else {
                LMud_Ringbuffer_SkipBytes(&self->outbuf, written);
            }
        }
    }

    if (LMud_Selector_IsExcept(selector, self->fd))
    {
        LMud_Debugf(self->net->mud, LMud_LogLevel_HALF_DEBUG, "FD(%d): Handling exception!", self->fd);
        LMud_Connection_HandleError(self);
    }

    if (self->closing && LMud_Ringbuffer_IsEmpty(&self->outbuf))
    {
        LMud_Connection_CloseImmediately(self);
    }
}


static void LMud_Connections_DeleteConnection(struct LMud_Connections* self, struct LMud_Connection* connection)
{
    (void) self;
    LMud_Connection_Destroy(connection);
    LMud_Free(connection);
}

void LMud_Connections_Create(struct LMud_Connections* self, struct LMud_Net* net)
{
    self->net         = net;
    self->connections = NULL;
}

void LMud_Connections_Destroy(struct LMud_Connections* self)
{
    while (self->connections != NULL)
    {
        LMud_Connections_DeleteConnection(self, self->connections);
    }
}

bool LMud_Connections_RegisterFileDescriptor(struct LMud_Connections* self, int fd, struct LMud_Connection** connection_location)
{
    struct LMud_Connection*  connection;

    connection = LMud_Alloc(sizeof(struct LMud_Connection));

    if (connection != NULL)
    {
        LMud_Connection_Create(connection, self->net, fd);
        LMud_Connection_Link(connection, &self->connections);
    }

    if (connection_location != NULL)
    {
        *connection_location = connection;
    }

    return connection != NULL;
}

bool LMud_Connections_RegisterFileDescriptorOrClose(struct LMud_Connections* self, int fd, struct LMud_Connection** connection)
{
    if (LMud_Connections_RegisterFileDescriptor(self, fd, connection))
        return true;
    else
    {
        close(fd);
        return false;
    }
}

void LMud_Connections_RegisterOnSelector(struct LMud_Connections* self, struct LMud_Selector* selector)
{
    struct LMud_Connection*  connection;

    for (connection = self->connections; connection != NULL; connection = connection->next)
    {
        LMud_Connection_RegisterOnSelector(connection, selector);
    }
}

void LMud_Connections_Tick(struct LMud_Connections* self, struct LMud_Selector* selector)
{
    struct LMud_Connection*  connection;

    for (connection = self->connections; connection != NULL; connection = connection->next)
    {
        LMud_Connection_Tick(connection, selector);
    }
}
