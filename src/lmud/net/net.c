
#include <lmud/glue.h>
#include <lmud/net/selector.h>

#include "net.h"


bool LMud_Net_Create(struct LMud_Net* self, struct LMud* mud)
{
    self->mud = mud;

    LMud_Servers_Create(&self->servers, self);
    LMud_Connections_Create(&self->connections, self);
    
    return true;
}

void LMud_Net_Destroy(struct LMud_Net* self)
{
    LMud_Connections_Destroy(&self->connections);
    LMud_Servers_Destroy(&self->servers);
}

void LMud_Net_Mark(struct LMud_GC* gc, struct LMud_Net* self)
{
    LMud_Debugf(self->mud, LMud_LogLevel_FULL_DEBUG, "Marking LMud_Net!");
    LMud_Servers_Mark(gc, &self->servers);
}

bool LMud_Net_OpenV4(struct LMud_Net* self, const char* address, LMud_Port port, LMud_Any startup_function)
{
    LMud_Logf(self->mud, LMud_LogLevel_INFO, "Opening IPv4 server on %s:%d...", address, port);
    return LMud_Servers_OpenV4(&self->servers, address, port, startup_function);
}

bool LMud_Net_OpenV6(struct LMud_Net* self, const char* address, LMud_Port port, LMud_Any startup_function)
{
    LMud_Logf(self->mud, LMud_LogLevel_INFO, "Opening IPv6 server on [%s]:%d...", address, port);
    return LMud_Servers_OpenV6(&self->servers, address, port, startup_function);
}

bool LMud_Net_RegisterClientFileDescriptor(struct LMud_Net* self, int fd, bool close_on_error, struct LMud_Connection** connection)
{
    if (close_on_error) {
        return LMud_Connections_RegisterFileDescriptorOrClose(&self->connections, fd, connection);
    } else {
        return LMud_Connections_RegisterFileDescriptor(&self->connections, fd, connection);
    }
}

bool LMud_Net_RegisterClientSocket(struct LMud_Net* self, LMud_Socket socket, bool close_on_error, struct LMud_Connection** connection)
{
    /*
     * Since LMud_Socket is just an alias for int, we can just forward
     * our socket to LMud_Net_RegisterClientFileDescriptor.
     */
    return LMud_Net_RegisterClientFileDescriptor(self, socket, close_on_error, connection);
}

bool LMud_Net_IncomingConnection(struct LMud_Net* self, struct LMud_Server* from, LMud_Socket socket)
{
    struct LMud_Connection*  connection;

    LMud_Logf(self->mud, LMud_LogLevel_NOTE, "Incoming connection via network!");

    if (LMud_Net_RegisterClientSocket(self, socket, true, &connection)) {
        LMud_NotifyIncomingConnection(self->mud, LMud_Server_GetStartupFunction(from), connection);
        return true;
    } else {
        return false;
    }
}

void LMud_Net_Tick(struct LMud_Net* self, bool block)
{
    struct LMud_Selector  selector;

    LMud_Debugf(self->mud, block ? LMud_LogLevel_FULL_DEBUG : LMud_LogLevel_ALL, "LMud_Net_Tick(block=%d)...%s", block, block ? " (block=0 is hidden)" : "");

    LMud_Selector_Create(&selector);
    {
        LMud_Servers_RegisterOnSelector(&self->servers, &selector);
        LMud_Connections_RegisterOnSelector(&self->connections, &selector);

        LMud_Selector_Select(&selector, block);

        LMud_Connections_Tick(&self->connections, &selector);
        LMud_Servers_Tick(&self->servers, &selector);
    }
    LMud_Selector_Destroy(&selector);
}
