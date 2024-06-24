
#pragma once

#include <lmud/lisp/base.h>

struct LMud_Scheduler
{
    struct LMud_Lisp*  lisp;
};

bool LMud_Scheduler_Create(struct LMud_Scheduler* self, struct LMud_Lisp* lisp);
void LMud_Scheduler_Destroy(struct LMud_Scheduler* self);

struct LMud_Fiber* LMud_Scheduler_SpawnFiber(struct LMud_Scheduler* self);
