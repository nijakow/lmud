
#include <lmud/decls.h>
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
    LMud_FiberQueue_Create(&self->zombies);

    return true;
}

void LMud_Scheduler_Destroy(struct LMud_Scheduler* self)
{
    LMud_FiberQueue_Destroy(&self->running_fibers);
    LMud_FiberQueue_Destroy(&self->zombies);

    if (self->fibers != NULL)
    {
        LMud_Logf(self->lisp->mud, LMud_LogLevel_WARNING, "Destroying scheduler with fibers still alive!");

        while (self->fibers != NULL)
        {
            LMud_Fiber_Unlink(self->fibers);
        }
    }
}

void LMud_Scheduler_Mark(struct LMud_GC* gc, struct LMud_Scheduler* self)
{
    struct LMud_Fiber*  fiber;

    LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Marking LMud_Scheduler!");

    for (fiber = self->fibers; fiber != NULL; fiber = fiber->next)
    {
        LMud_Fiber_Mark(gc, fiber);
    }
}


struct LMud_Fiber* LMud_Scheduler_GetAllFibers_UNSAFE(struct LMud_Scheduler* self)
{
    return self->fibers;
}


struct LMud_Fiber* LMud_Scheduler_SpawnFiber(struct LMud_Scheduler* self, struct LMud_Profile* profile)
{
    struct LMud_Fiber*  fiber;

    fiber = LMud_Alloc(sizeof(struct LMud_Fiber));

    if (fiber != NULL)
    {
        LMud_Fiber_Create(fiber, self->lisp, self, profile);
        LMud_Fiber_Link(fiber, &self->fibers);
        LMud_Debugf(self->lisp->mud, LMud_LogLevel_HALF_DEBUG, "Created fiber %p ...", fiber);
    }

    return fiber;
}

bool LMud_Scheduler_TryDeleteFiber(struct LMud_Scheduler* self, struct LMud_Fiber* fiber)
{
    if (LMud_Fiber_IsReadyForDeletion(fiber)) {
        LMud_Debugf(self->lisp->mud, LMud_LogLevel_HALF_DEBUG, "Deleting fiber %p ...", fiber);
        LMud_Fiber_Destroy(fiber);
        LMud_Free(fiber);
        return true;
    } else {
        LMud_Debugf(self->lisp->mud, LMud_LogLevel_WARNING, "Deletion of fiber %p has been deactivated!", fiber);
        return false;
    }
}

void LMud_Scheduler_RequestDeleteFiber(struct LMud_Scheduler* self, struct LMud_Fiber* fiber)
{
    (void) self;

    LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Requesting a delete of fiber %p ...", fiber);

    LMud_Scheduler_TryDeleteFiber(self, fiber);
}

void LMud_Scheduler_MoveToRunningQueue(struct LMud_Scheduler* self, struct LMud_Fiber* fiber)
{
    LMud_Fiber_MoveToQueue(fiber, &self->running_fibers);
}

struct LMud_Fiber* LMud_Scheduler_Kickstart(struct LMud_Scheduler* self, struct LMud_Profile* profile, LMud_Any thunk)
{
    struct LMud_Fiber*  fiber;

    fiber = LMud_Scheduler_SpawnFiber(self, profile);

    if (fiber != NULL)
    {
        LMud_Fiber_EnterThunk(fiber, thunk);
        LMud_Fiber_ControlStart(fiber);
    }

    return fiber;
}

struct LMud_Fiber* LMud_Scheduler_KickstartWithArgs(struct LMud_Scheduler* self, struct LMud_Profile* profile, LMud_Any function, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_Fiber*  fiber;

    fiber = LMud_Scheduler_SpawnFiber(self, profile);

    if (fiber != NULL)
    {
        LMud_Fiber_Enter(fiber, function, arguments, argument_count);
        LMud_Fiber_ControlStart(fiber);
    }

    return fiber;
}


static void LMud_Scheduler_FiberTerminated(struct LMud_Scheduler* self, struct LMud_Fiber* fiber)
{
    LMud_Debugf(self->lisp->mud, LMud_LogLevel_HALF_DEBUG, "Setting fiber %p to terminated.", fiber);
    LMud_Fiber_FinalizeTerminate_FRIEND(fiber);
    LMud_Scheduler_RequestDeleteFiber(self, fiber);
}

static void LMud_Scheduler_MakeZombie(struct LMud_Scheduler* self, struct LMud_Fiber* fiber)
{
    LMud_Debugf(self->lisp->mud, LMud_LogLevel_HALF_DEBUG, "Moving fiber %p to the zombie queue.", fiber);
    LMud_Fiber_MoveToQueue(fiber, &self->zombies);
}

static bool LMud_Scheduler_TickFiber(struct LMud_Scheduler* self, struct LMud_Fiber* fiber)
{
    (void) self;

    if (!LMud_Fiber_HasFrames(fiber))
    {
        LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Fiber %p has no frames left.", fiber);
        LMud_Scheduler_MakeZombie(self, fiber);
        return false;
    }

    if (LMud_Fiber_IsYielding(fiber))
        LMud_Fiber_ControlUnyield(fiber);

    assert(LMud_Fiber_IsRunning(fiber));

    LMud_Fiber_Tick(fiber);

    return true;
}

bool LMud_Scheduler_BlockAndRunThunk(struct LMud_Scheduler* self, struct LMud_Profile* profile, LMud_Any thunk, LMud_Any* result)
{
    struct LMud_Fiber*  fiber;

    fiber = LMud_Scheduler_SpawnFiber(self, profile);

    LMud_Debugf(self->lisp->mud, LMud_LogLevel_DEBUG, "Block-running fiber %p ...");

    if (fiber == NULL)
        return false;

    LMud_Fiber_EnterThunk(fiber, thunk);

    LMud_Fiber_ControlStart(fiber);
    
    while (LMud_Fiber_IsRunning(fiber) && LMud_Scheduler_TickFiber(self, fiber))
    {
        LMud_Lisp_PeriodicInterrupt(self->lisp);
    }

    LMud_Debugf(self->lisp->mud, LMud_LogLevel_HALF_DEBUG, "Block-running of fiber %p has stopped.");

    /*
     * Yielding and Waiting are illegal operations in this run mode.
     * If any of these operations are attempted, the fiber will be terminated
     * withouth any further processing.
     */

    if (!LMud_Fiber_IsRunning(fiber))
    {
        LMud_Debugf(self->lisp->mud, LMud_LogLevel_WARNING, "Fiber %p has completed execution with an invalid state: %d!", fiber, LMud_Fiber_GetState(fiber));
    }

    if (result != NULL)
        *result = LMud_Fiber_GetAccumulator(fiber);

    LMud_Scheduler_RequestDeleteFiber(self, fiber);

    return true;
}

bool LMud_Scheduler_NeedsControlBackImmediately(struct LMud_Scheduler* self)
{
    return LMud_FiberQueue_HasFibers(&self->running_fibers);
}

void LMud_Scheduler_TickRunning(struct LMud_Scheduler* self)
{
    struct LMud_Fiber*  fiber;
    struct LMud_Fiber*  next;

    fiber = self->running_fibers.fibers;

    LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Ticking fibers ...", fiber);

    while (fiber != NULL)
    {
        LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Ticking fiber %p ...", fiber);

        next = fiber->queue_next;

        LMud_Scheduler_TickFiber(self, fiber);

        fiber = next;
    }
}

void LMud_Scheduler_TickZombies(struct LMud_Scheduler* self)
{
    struct LMud_Fiber*  fiber;
    struct LMud_Fiber*  next;

    fiber = self->zombies.fibers;

    LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Ticking zombies ...");

    while (fiber != NULL)
    {
        LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Ticking zombie %p ...", fiber);

        next = fiber->queue_next;

        if (!LMud_Fiber_HasFrames(fiber)) {
            LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Zombie %p is terminated.", fiber);
            LMud_Scheduler_FiberTerminated(self, fiber);
        } else {
            LMud_Debugf(self->lisp->mud, LMud_LogLevel_ERROR, "Zombie %p is still alive.", fiber);
        }

        fiber = next;
    }
}

void LMud_Scheduler_Tick(struct LMud_Scheduler* self)
{
    LMud_Scheduler_TickRunning(self);
    LMud_Scheduler_TickZombies(self);
}
