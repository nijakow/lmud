
#include <lmud/lisp/runtime/fiber.h>
#include <lmud/util/memory.h>

#include "scheduler.h"

bool LMud_Scheduler_Create(struct LMud_Scheduler* self, struct LMud_Lisp* lisp)
{
    self->lisp = lisp;

    return true;
}

void LMud_Scheduler_Destroy(struct LMud_Scheduler* self)
{
    (void) self;
}


struct LMud_Fiber* LMud_Scheduler_SpawnFiber(struct LMud_Scheduler* self)
{
    struct LMud_Fiber*  fiber;

    fiber = LMud_Alloc(sizeof(struct LMud_Fiber));

    if (fiber != NULL)
    {
        LMud_Fiber_Create(fiber, self->lisp);
    }

    return fiber;
}
