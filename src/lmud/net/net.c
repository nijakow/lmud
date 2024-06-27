
#include <lmud/net/selector.h>

#include "net.h"


void LMud_Net_Create(struct LMud_Net* self)
{
    LMud_Servers_Create(&self->servers, self);
    LMud_Connections_Create(&self->connections);
}

void LMud_Net_Destroy(struct LMud_Net* self)
{
    LMud_Connections_Destroy(&self->connections);
    LMud_Servers_Destroy(&self->servers);
}

bool LMud_Net_OpenV4(struct LMud_Net* self, const char* address, LMud_Port port)
{
    return LMud_Servers_OpenV4(&self->servers, address, port);
}

bool LMud_Net_OpenV6(struct LMud_Net* self, const char* address, LMud_Port port)
{
    return LMud_Servers_OpenV6(&self->servers, address, port);
}

void LMud_Net_RegisterClientFileDescriptor(struct LMud_Net* self, int fd)
{
    (void) self;
    (void) fd;
}

void LMud_Net_RegisterClientSocket(struct LMud_Net* self, LMud_Socket socket)
{
    LMud_Net_RegisterClientFileDescriptor(self, socket);
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
