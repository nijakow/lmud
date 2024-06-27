
#pragma once

#include <lmud/defs.h>

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
