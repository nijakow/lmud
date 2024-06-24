
#pragma once

#include <lmud/defs.h>
#include <lmud/lisp/lisp.h>
#include <lmud/lisp/runtime/scheduler.h>

struct LMud
{
    struct LMud_Lisp       lisp;
    struct LMud_Scheduler  scheduler;
};

bool LMud_Create(struct LMud* self);
void LMud_Destroy(struct LMud* self);

void LMud_Main(struct LMud* self, int argc, char* argv[]);
