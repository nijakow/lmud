
#include <lmud/net/net.h>
#include <lmud/util/inet.h>
#include <lmud/util/memory.h>

#include "servers.h"

void LMud_Servers_Create(struct LMud_Servers* self, struct LMud_Net* net)
{
    self->net   = net;
    self->fds   = NULL;
    self->alloc = 0;
    self->fill  = 0;
}

void LMud_Servers_Destroy(struct LMud_Servers* self)
{
    LMud_Size  index;

    for (index = 0; index < self->fill; index++)
    {
        close(self->fds[index]);
    }

    LMud_Free(self->fds);
}

void LMud_Servers_RegisterOnSelector(struct LMud_Servers* self, struct LMud_Selector* selector)
{
    LMud_Size  index;

    for (index = 0; index < self->fill; index++)
    {
        LMud_Selector_AddRead(selector, self->fds[index]);
        LMud_Selector_AddExcept(selector, self->fds[index]);
    }
}

static void LMud_Servers_CloseByIndex(struct LMud_Servers* self, LMud_Size index)
{
    if (index < self->fill)
    {
        LMud_Inet_Close(self->fds[index]);
        self->fill--;
        if (index < self->fill)
            self->fds[index] = self->fds[self->fill];
    }
}

static void LMud_Servers_HandleReadByIndex(struct LMud_Servers* self, LMud_Size index)
{
    struct LMud_Inet_AcceptInfo  info;

    LMud_Inet_AcceptInfo_Create(&info);
    {
        if (LMud_Inet_Accept(self->fds[index], &info)) {
            LMud_Net_RegisterClientSocket(self->net, LMud_Inet_AcceptInfo_GetSocket(&info));
        }
    }
    LMud_Inet_AcceptInfo_Destroy(&info);
}

void LMud_Servers_Tick(struct LMud_Servers* self, struct LMud_Selector* selector)
{
    LMud_Size  index;

    for (index = 0; index < self->fill; index++)
    {
        if (LMud_Selector_IsRead(selector, self->fds[index]))
        {
            LMud_Servers_HandleReadByIndex(self, index);
        }

        if (LMud_Selector_IsExcept(selector, self->fds[index]))
        {
            LMud_Servers_CloseByIndex(self, index);
        }
    }
}
