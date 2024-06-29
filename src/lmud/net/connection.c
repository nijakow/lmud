
#include <lmud/util/memory.h>

#include "connection.h"


void LMud_Connection_Create(struct LMud_Connection* self, int fd)
{
    self->fd   = fd;

    self->prev = NULL;
    self->next = NULL;

    LMud_Ringbuffer_Create(&self->inbuf,  4096);
    LMud_Ringbuffer_Create(&self->outbuf, 4096);
}

void LMud_Connection_Destroy(struct LMud_Connection* self)
{
    close(self->fd);
    LMud_Connection_Unlink(self);
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
}

void LMud_Connection_RegisterOnSelector(struct LMud_Connection* self, struct LMud_Selector* selector)
{
    if (LMud_Ringbuffer_HasData(&self->outbuf))
        LMud_Selector_AddWrite(selector, self->fd);

    LMud_Selector_AddRead(selector, self->fd);
    LMud_Selector_AddExcept(selector, self->fd);
}

void LMud_Connection_HandleError(struct LMud_Connection* self)
{
    (void) self;
}

void LMud_Connection_HandleDisconnect(struct LMud_Connection* self)
{
    LMud_Connection_HandleError(self);
}

void LMud_Connection_Tick(struct LMud_Connection* self, struct LMud_Selector* selector)
{
    size_t   size;
    ssize_t  ssize;
    ssize_t  written;
    char     buffer[1024];

    if (LMud_Selector_IsRead(selector, self->fd))
    {
        ssize = read(self->fd, buffer, sizeof(buffer));

        if (ssize <= 0) {
            LMud_Connection_HandleDisconnect(self);
        } else {
            LMud_Ringbuffer_WriteBytes(&self->inbuf, buffer, ssize);
        }
    }

    if (LMud_Selector_IsWrite(selector, self->fd))
    {
        written = 0;
        size    = LMud_Ringbuffer_PeekBytes(&self->outbuf, buffer, sizeof(buffer));

        if (size > 0)
        {
            written = write(self->fd, buffer, size);
        }

        if (written < 0) {
            LMud_Connection_HandleDisconnect(self);
        } else {
            LMud_Ringbuffer_SkipBytes(&self->outbuf, written);
        }
    }

    if (LMud_Selector_IsExcept(selector, self->fd))
    {
        LMud_Connection_HandleError(self);
    }
}


static void LMud_Connections_DeleteConnection(struct LMud_Connections* self, struct LMud_Connection* connection)
{
    (void) self;
    LMud_Connection_Destroy(connection);
    LMud_Free(connection);
}

void LMud_Connections_Create(struct LMud_Connections* self)
{
    self->connections = NULL;
}

void LMud_Connections_Destroy(struct LMud_Connections* self)
{
    while (self->connections != NULL)
    {
        LMud_Connections_DeleteConnection(self, self->connections);
    }
}

bool LMud_Connections_RegisterFileDescriptor(struct LMud_Connections* self, int fd)
{
    struct LMud_Connection*  connection;

    connection = LMud_Alloc(sizeof(struct LMud_Connection));

    if (connection != NULL)
    {
        LMud_Connection_Create(connection, fd);
        LMud_Connection_Link(connection, &self->connections);
    }

    return connection != NULL;
}

bool LMud_Connections_RegisterFileDescriptorOrClose(struct LMud_Connections* self, int fd)
{
    if (LMud_Connections_RegisterFileDescriptor(self, fd))
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
