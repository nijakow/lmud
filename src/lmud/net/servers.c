
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
