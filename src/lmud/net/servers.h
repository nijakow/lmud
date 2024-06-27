
#pragma once

#include <lmud/defs.h>

struct LMud_Servers
{
    int*       fds;
    LMud_Size  alloc;
    LMud_Size  fill;
};

void LMud_Servers_Create(struct LMud_Servers* self);
void LMud_Servers_Destroy(struct LMud_Servers* self);
