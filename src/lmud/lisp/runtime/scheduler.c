
#include <lmud/lisp/gc.h>
#include <lmud/lisp/lisp.h>
#include <lmud/lisp/runtime/fiber.h>
#include <lmud/util/memory.h>

#include "scheduler.h"

bool LMud_Scheduler_Create(struct LMud_Scheduler* self, struct LMud_Lisp* lisp)
{
    self->lisp   = lisp;
    self->fibers = NULL;

    LMud_FiberQueue_Create(&self->running_fibers);

    return true;
}

void LMud_Scheduler_Destroy(struct LMud_Scheduler* self)
{
    LMud_FiberQueue_Destroy(&self->running_fibers);

    if (self->fibers != NULL)
    {
        printf("Warning: Destroying scheduler with fibers still alive.\n");

        while (self->fibers != NULL)
        {
            LMud_Fiber_Unlink(self->fibers);
        }
    }
}

void LMud_Scheduler_Mark(struct LMud_GC* gc, struct LMud_Scheduler* self)
{
    struct LMud_Fiber*  fiber;

    for (fiber = self->fibers; fiber != NULL; fiber = fiber->next)
    {
        LMud_Fiber_Mark(gc, fiber);
    }
}


struct LMud_Fiber* LMud_Scheduler_SpawnFiber(struct LMud_Scheduler* self)
{
    struct LMud_Fiber*  fiber;

    fiber = LMud_Alloc(sizeof(struct LMud_Fiber));

    if (fiber != NULL)
    {
        LMud_Fiber_Create(fiber, self->lisp, self);
        LMud_Fiber_Link(fiber, &self->fibers);
    }

    return fiber;
}

void LMud_Scheduler_RequestDeleteFiber(struct LMud_Scheduler* self, struct LMud_Fiber* fiber)
{
    (void) self;

    LMud_Fiber_Destroy(fiber);
    LMud_Free(fiber);
}

struct LMud_Fiber* LMud_Scheduler_Kickstart(struct LMud_Scheduler* self, LMud_Any thunk)
{
    struct LMud_Fiber*  fiber;

    fiber = LMud_Scheduler_SpawnFiber(self);

    if (fiber != NULL)
    {
        LMud_Fiber_EnterThunk(fiber, thunk);
        LMud_Fiber_MoveToQueue(fiber, &self->running_fibers);
    }

    return fiber;
}

struct LMud_Fiber* LMud_Scheduler_KickstartWithArgs(struct LMud_Scheduler* self, LMud_Any function, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_Fiber*  fiber;

    fiber = LMud_Scheduler_SpawnFiber(self);

    if (fiber != NULL)
    {
        LMud_Fiber_Enter(fiber, function, arguments, argument_count);
        LMud_Fiber_MoveToQueue(fiber, &self->running_fibers);
    }

    return fiber;
}

bool LMud_Scheduler_BlockAndRunThunk(struct LMud_Scheduler* self, LMud_Any thunk, LMud_Any* result)
{
    struct LMud_Fiber*  fiber;

    fiber = LMud_Scheduler_SpawnFiber(self);

    if (fiber == NULL)
        return false;

    LMud_Fiber_EnterThunk(fiber, thunk);
    
    while (!LMud_Fiber_HasTerminated(fiber))
    {
        LMud_Fiber_Tick(fiber);
        LMud_Lisp_PeriodicInterrupt(self->lisp);
    }

    if (result != NULL)
        *result = LMud_Fiber_GetAccumulator(fiber);

    LMud_Scheduler_RequestDeleteFiber(self, fiber);

    return true;
}

void LMud_Scheduler_Tick(struct LMud_Scheduler* self)
{
    struct LMud_Fiber*  fiber;
    struct LMud_Fiber*  next;

    fiber = self->running_fibers.fibers;

    while (fiber != NULL)
    {
        next = fiber->queue_next;

        LMud_Fiber_Tick(fiber);

        if (LMud_Fiber_HasTerminated(fiber))
        {
            printf("[Note]: Fiber terminated.\n");
            LMud_Fiber_UnlinkQueue(fiber);
            LMud_Scheduler_RequestDeleteFiber(self, fiber);
        }

        fiber = next;
    }
}
