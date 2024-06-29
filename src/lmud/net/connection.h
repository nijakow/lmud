
#pragma once

#include <lmud/defs.h>
#include <lmud/net/selector.h>
#include <lmud/util/ringbuffer.h>

struct LMud_Connection
{
    int  fd;

    struct LMud_Connection**  prev;
    struct LMud_Connection*   next;

    struct LMud_Ringbuffer    inbuf;
    struct LMud_Ringbuffer    outbuf;
};

void LMud_Connection_Create(struct LMud_Connection* self, int fd);
void LMud_Connection_Destroy(struct LMud_Connection* self);

void LMud_Connection_Link(struct LMud_Connection* self, struct LMud_Connection** list);
void LMud_Connection_Unlink(struct LMud_Connection* self);

void LMud_Connection_RegisterOnSelector(struct LMud_Connection* self, struct LMud_Selector* selector);


struct LMud_Connections
{
    struct LMud_Connection*  connections;
};

void LMud_Connections_Create(struct LMud_Connections* self);
void LMud_Connections_Destroy(struct LMud_Connections* self);

bool LMud_Connections_RegisterFileDescriptor(struct LMud_Connections* self, int fd);
bool LMud_Connections_RegisterFileDescriptorOrClose(struct LMud_Connections* self, int fd);

void LMud_Connections_RegisterOnSelector(struct LMud_Connections* self, struct LMud_Selector* selector);

void LMud_Connections_Tick(struct LMud_Connections* self, struct LMud_Selector* selector);
