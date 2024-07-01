
#include <lmud/net/net.h>
#include <lmud/util/inet.h>
#include <lmud/util/memory.h>

#include "servers.h"


void LMud_Server_Create(struct LMud_Server* self, struct LMud_Net* net, LMud_Socket fd)
{
    self->net  = net;
    self->prev = NULL;
    self->next = NULL;
    self->fd   = fd;
}

void LMud_Server_Destroy(struct LMud_Server* self)
{
    LMud_Server_Unlink(self);
    LMud_Inet_Close(self->fd);
}


struct LMud_Server* LMud_Server_New(struct LMud_Net* net, LMud_Socket fd)
{
    struct LMud_Server*  self;

    self = LMud_Alloc(sizeof(struct LMud_Server));

    if (self != NULL)
    {
        LMud_Server_Create(self, net, fd);
    }
    
    return self;
}

void LMud_Server_Delete(struct LMud_Server* self)
{
    LMud_Server_Destroy(self);
    LMud_Free(self);
}


void LMud_Server_Link(struct LMud_Server* self, struct LMud_Server** list)
{
    LMud_Server_Unlink(self);

    self->prev =  list;
    self->next = *list;

    if (*list != NULL)
        (*list)->prev = &self->next;
    
    *list = self;
}

void LMud_Server_Unlink(struct LMud_Server* self)
{
    if (self->prev != NULL)
        *self->prev = self->next;
    
    if (self->next != NULL)
        self->next->prev = self->prev;
}

void LMud_Server_RegisterOnSelector(struct LMud_Server* self, struct LMud_Selector* selector)
{
    LMud_Selector_AddRead(selector, self->fd);
    LMud_Selector_AddExcept(selector, self->fd);
}

void LMud_Server_HandleRead(struct LMud_Server* self)
{
    struct LMud_Inet_AcceptInfo  info;

    LMud_Inet_AcceptInfo_Create(&info);
    {
        if (LMud_Inet_Accept(self->fd, &info))
        {
            LMud_Net_IncomingConnection(self->net, LMud_Inet_AcceptInfo_GetSocket(&info));
        }
    }
    LMud_Inet_AcceptInfo_Destroy(&info);
}

void LMud_Server_Tick(struct LMud_Server* self, struct LMud_Selector* selector)
{
    if (LMud_Selector_IsRead(selector, self->fd))
    {
        LMud_Server_HandleRead(self);
    }

    if (LMud_Selector_IsExcept(selector, self->fd))
    {
        // TODO!
        printf("LMud_Server_Tick: Exception!\n");
    }
}


void LMud_Servers_Create(struct LMud_Servers* self, struct LMud_Net* net)
{
    self->net     = net;
    self->servers = NULL;
}

void LMud_Servers_Destroy(struct LMud_Servers* self)
{
    while (self->servers != NULL)
    {
        LMud_Server_Delete(self->servers);
    }
}

static bool LMud_Servers_PushSocket(struct LMud_Servers* self, LMud_Socket socket)
{
    struct LMud_Server*  server;

    server = LMud_Server_New(self->net, socket);

    if (server != NULL)
    {
        LMud_Server_Link(server, &self->servers);
    }

    return (server != NULL);
}

static void LMud_Servers_PushSocketOrClose(struct LMud_Servers* self, LMud_Socket socket)
{
    if (!LMud_Servers_PushSocket(self, socket))
        LMud_Inet_Close(socket);
}

bool LMud_Servers_OpenV4(struct LMud_Servers* self, const char* address, LMud_Port port)
{
    LMud_Socket  socket;

    if (LMud_Inet_OpenServerV4(address, port, &socket)) {
        LMud_Servers_PushSocketOrClose(self, socket);
        return true;
    } else {
        return false;
    }
}

bool LMud_Servers_OpenV6(struct LMud_Servers* self, const char* address, LMud_Port port)
{
    LMud_Socket  socket;

    if (LMud_Inet_OpenServerV6(address, port, &socket)) {
        LMud_Servers_PushSocketOrClose(self, socket);
        return true;
    } else {
        return false;
    }
}

void LMud_Servers_RegisterOnSelector(struct LMud_Servers* self, struct LMud_Selector* selector)
{
    struct LMud_Server*  server;

    for (server = self->servers; server != NULL; server = server->next)
    {
        LMud_Server_RegisterOnSelector(server, selector);
    }
}


void LMud_Servers_Tick(struct LMud_Servers* self, struct LMud_Selector* selector)
{
    struct LMud_Server*  server;
    struct LMud_Server*  next;

    for (server = self->servers; server != NULL; server = next)
    {
        next = server->next;

        LMud_Server_Tick(server, selector);
    }
}
