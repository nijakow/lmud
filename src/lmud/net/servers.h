
#pragma once

#include <lmud/defs.h>
#include <lmud/net/selector.h>

struct LMud_Servers
{
    struct LMud_Net*  net;
    LMud_Socket*      fds;
    LMud_Size         alloc;
    LMud_Size         fill;
};

void LMud_Servers_Create(struct LMud_Servers* self, struct LMud_Net* net);
void LMud_Servers_Destroy(struct LMud_Servers* self);

bool LMud_Servers_OpenV4(struct LMud_Servers* self, const char* address, LMud_Port port);
bool LMud_Servers_OpenV6(struct LMud_Servers* self, const char* address, LMud_Port port);

void LMud_Servers_RegisterOnSelector(struct LMud_Servers* self, struct LMud_Selector* selector);

void LMud_Servers_Tick(struct LMud_Servers* self, struct LMud_Selector* selector);
