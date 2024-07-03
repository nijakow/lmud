
#include <lmud/lisp/lisp.h>

#include "process.h"


void LMud_Process_Create(struct LMud_Process* self, struct LMud_Fiber* fiber)
{
    self->fiber = fiber;
}

void LMud_Process_Destroy(struct LMud_Process* self)
{
    (void) self;
}

void LMud_Process_Mark(struct LMud_GC* gc, struct LMud_Process* self)
{
    // Nothing to do, fibers are marked by the scheduler
    (void) gc;
    (void) self;
}

LMud_Size LMud_Process_CalculateSizeInBytes(struct LMud_Process* self)
{
    (void) self;
    return sizeof(struct LMud_Process);
}


LMud_Any LMud_Process_GetStateAsLispValue(struct LMud_Process* self, struct LMud_Lisp* lisp)
{
    switch (LMud_Fiber_GetState(self->fiber))
    {
        case LMud_FiberState_CREATED:    return LMud_Lisp_InternKeyword(lisp, "CREATED");
        case LMud_FiberState_RUNNING:    return LMud_Lisp_InternKeyword(lisp, "RUNNING");
        case LMud_FiberState_WAITING:    return LMud_Lisp_InternKeyword(lisp, "WAITING");
        case LMud_FiberState_YIELDING:   return LMud_Lisp_InternKeyword(lisp, "YIELDING");
        case LMud_FiberState_TERMINATED: return LMud_Lisp_InternKeyword(lisp, "TERMINATED");
        default: return LMud_Lisp_Nil(lisp);
    }
}
