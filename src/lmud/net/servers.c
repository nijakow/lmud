
#include <lmud/util/memory.h>

#include "servers.h"

void LMud_Servers_Create(struct LMud_Servers* self)
{
    self->fds   = NULL;
    self->alloc = 0;
    self->fill  = 0;
}

void LMud_Servers_Destroy(struct LMud_Servers* self)
{
    LMud_Size  index;

    for (index = 0; index < self->fill; ++index)
    {
        close(self->fds[index]);
    }

    LMud_Free(self->fds);
}

void LMud_Servers_RegisterOnSelector(struct LMud_Servers* self, struct LMud_Selector* selector)
{
    LMud_Size  index;

    for (index = 0; index < self->fill; ++index)
    {
        LMud_Selector_AddRead(selector, self->fds[index]);
        LMud_Selector_AddExcept(selector, self->fds[index]);
    }
}

void LMud_Servers_Tick(struct LMud_Servers* self, struct LMud_Selector* selector)
{
    LMud_Size  index;

    for (index = 0; index < self->fill; ++index)
    {
        if (LMud_Selector_IsRead(selector, self->fds[index]))
        {
            // Handle read
        }

        if (LMud_Selector_IsExcept(selector, self->fds[index]))
        {
            // Handle exception
        }
    }
}
