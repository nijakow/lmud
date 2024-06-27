
#pragma once

#include <lmud/defs.h>
#include <lmud/net/selector.h>

struct LMud_Connection
{
    int  fd;

    struct LMud_Connection**  prev;
    struct LMud_Connection*   next;
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

void LMud_Connections_RegisterOnSelector(struct LMud_Connections* self, struct LMud_Selector* selector);

void LMud_Connections_Tick(struct LMud_Connections* self, struct LMud_Selector* selector);
