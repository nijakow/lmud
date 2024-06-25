
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

void LMud_Scheduler_RequestDeleteFiber(struct LMud_Scheduler* self, struct LMud_Fiber* fiber)
{
    (void) self;

    LMud_Fiber_Destroy(fiber);
    LMud_Free(fiber);
}

bool LMud_Scheduler_BlockAndRunThunk(struct LMud_Scheduler* self, LMud_Any thunk, LMud_Any* result)
{
    struct LMud_Fiber*  fiber;

    fiber = LMud_Scheduler_SpawnFiber(self);

    if (fiber == NULL)
        return false;

    LMud_Fiber_EnterThunk(fiber, thunk);
    LMud_Fiber_Tick(fiber);

    if (result != NULL)
        *result = LMud_Fiber_GetAccumulator(fiber);

    LMud_Scheduler_RequestDeleteFiber(self, fiber);

    return true;
}
