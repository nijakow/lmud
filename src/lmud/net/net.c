
#include <lmud/net/selector.h>

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

bool LMud_Net_OpenV4(struct LMud_Net* self, const char* address, LMud_Port port)
{
    // TODO

    (void) self;
    (void) address;
    (void) port;

    return false;
}

bool LMud_Net_OpenV6(struct LMud_Net* self, const char* address, LMud_Port port)
{
    // TODO

    (void) self;
    (void) address;
    (void) port;

    return false;
}

void LMud_Net_Tick(struct LMud_Net* self)
{
    struct LMud_Selector  selector;

    LMud_Selector_Create(&selector);
    {
        LMud_Servers_RegisterOnSelector(&self->servers, &selector);
        LMud_Connections_RegisterOnSelector(&self->connections, &selector);

        LMud_Selector_Select(&selector, true);

        LMud_Connections_Tick(&self->connections, &selector);
        LMud_Servers_Tick(&self->servers, &selector);
    }
    LMud_Selector_Destroy(&selector);
}
