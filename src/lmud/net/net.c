
#include "net.h"


void LMud_Net_Create(struct LMud_Net* self)
{
    LMud_Servers_Create(&self->servers);
    LMud_Connections_Create(&self->connections);
}

void LMud_Net_Destroy(struct LMud_Net* self)
{
    LMud_Connections_Destroy(&self->connections);
    LMud_Servers_Destroy(&self->servers);
}
