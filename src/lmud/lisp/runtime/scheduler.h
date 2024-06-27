
#pragma once

#include <lmud/lisp/base.h>

struct LMud_Scheduler
{
    struct LMud_Lisp*   lisp;
    struct LMud_Fiber*  fibers;
};

bool LMud_Scheduler_Create(struct LMud_Scheduler* self, struct LMud_Lisp* lisp);
void LMud_Scheduler_Destroy(struct LMud_Scheduler* self);
void LMud_Scheduler_Mark(struct LMud_GC* gc, struct LMud_Scheduler* self);

struct LMud_Fiber* LMud_Scheduler_SpawnFiber(struct LMud_Scheduler* self);
void               LMud_Scheduler_RequestDeleteFiber(struct LMud_Scheduler* self, struct LMud_Fiber* fiber);

bool LMud_Scheduler_BlockAndRunThunk(struct LMud_Scheduler* self, LMud_Any thunk, LMud_Any* result);
