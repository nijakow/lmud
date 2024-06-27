
#pragma once

#include <lmud/defs.h>
#include <lmud/net/connection.h>
#include <lmud/net/servers.h>

struct LMud_Net
{
    struct LMud_Servers      servers;
    struct LMud_Connection*  connections;
};

void LMud_Net_Create(struct LMud_Net* self);
void LMud_Net_Destroy(struct LMud_Net* self);
