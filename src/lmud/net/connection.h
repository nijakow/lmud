
#pragma once

#include <lmud/defs.h>
#include <lmud/net/selector.h>
#include <lmud/lisp/runtime/fiber.h>
#include <lmud/util/ringbuffer.h>

struct LMud_ConnectionRef
{
    struct LMud_Connection*      connection;
    struct LMud_ConnectionRef**  prev;
    struct LMud_ConnectionRef*   next;
};

void LMud_ConnectionRef_Create(struct LMud_ConnectionRef* self, struct LMud_Connection* connection);
void LMud_ConnectionRef_Destroy(struct LMud_ConnectionRef* self);

void LMud_ConnectionRef_Link(struct LMud_ConnectionRef* self, struct LMud_ConnectionRef** list);
void LMud_ConnectionRef_Unlink(struct LMud_ConnectionRef* self);

void LMud_ConnectionRef_Kill(struct LMud_ConnectionRef* self);


struct LMud_Connection
{
    int  fd;

    struct LMud_Connection**    prev;
    struct LMud_Connection*     next;
    struct LMud_ConnectionRef*  refs;

    struct LMud_FiberQueue      waiting_fibers;

    struct LMud_Ringbuffer      inbuf;
    struct LMud_Ringbuffer      outbuf;
};

void LMud_Connection_Create(struct LMud_Connection* self, int fd);
void LMud_Connection_Destroy(struct LMud_Connection* self);

void LMud_Connection_Link(struct LMud_Connection* self, struct LMud_Connection** list);
void LMud_Connection_Unlink(struct LMud_Connection* self);

void LMud_Connection_RegisterOnSelector(struct LMud_Connection* self, struct LMud_Selector* selector);

void LMud_Connection_AddWaitingFiber(struct LMud_Connection* self, struct LMud_Fiber* fiber);
void LMud_Connection_ReadByte(struct LMud_Connection* self, struct LMud_Fiber* fiber);


struct LMud_Connections
{
    struct LMud_Connection*  connections;
};

void LMud_Connections_Create(struct LMud_Connections* self);
void LMud_Connections_Destroy(struct LMud_Connections* self);

bool LMud_Connections_RegisterFileDescriptor(struct LMud_Connections* self, int fd, struct LMud_Connection** connection);
bool LMud_Connections_RegisterFileDescriptorOrClose(struct LMud_Connections* self, int fd, struct LMud_Connection** connection);

void LMud_Connections_RegisterOnSelector(struct LMud_Connections* self, struct LMud_Selector* selector);

void LMud_Connections_Tick(struct LMud_Connections* self, struct LMud_Selector* selector);
