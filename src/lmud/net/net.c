
#include <lmud/util/memory.h>

#include "net.h"


static void LMud_Net_DeleteConnection(struct LMud_Net* self, struct LMud_Connection* connection)
{
    (void) self;
    LMud_Connection_Destroy(connection);
    LMud_Free(connection);
}


void LMud_Net_Create(struct LMud_Net* self)
{
    LMud_Servers_Create(&self->servers);
    self->connections = NULL;
}

void LMud_Net_Destroy(struct LMud_Net* self)
{
    while (self->connections != NULL)
    {
        LMud_Net_DeleteConnection(self, self->connections);
    }

    LMud_Servers_Destroy(&self->servers);
}
