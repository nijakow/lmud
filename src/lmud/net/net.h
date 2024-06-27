
#pragma once

#include <lmud/defs.h>
#include <lmud/net/connection.h>
#include <lmud/net/servers.h>

struct LMud_Net
{
    struct LMud_Servers      servers;
    struct LMud_Connections  connections;
};

void LMud_Net_Create(struct LMud_Net* self);
void LMud_Net_Destroy(struct LMud_Net* self);

bool LMud_Net_OpenV4(struct LMud_Net* self, const char* address, LMud_Port port);
bool LMud_Net_OpenV6(struct LMud_Net* self, const char* address, LMud_Port port);

void LMud_Net_Tick(struct LMud_Net* self);
