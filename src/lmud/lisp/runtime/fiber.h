
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/lisp/runtime/frame.h>

#define LMud_Fiber_MAX_ACCUMULATORS 16

struct LMud_Fiber
{
    struct LMud_Lisp*   lisp;

    struct LMud_Frame*  top;
    char*               stack;
    char*               stack_roof;
    char*               stack_pointer;

    LMud_Size           accumulator_count;
    LMud_Any            accumulator[LMud_Fiber_MAX_ACCUMULATORS];
};

void LMud_Fiber_Create(struct LMud_Fiber* self, struct LMud_Lisp* lisp);
void LMud_Fiber_Destroy(struct LMud_Fiber* self);

LMud_Any LMud_Fiber_GetAccumulator(struct LMud_Fiber* self);
void     LMud_Fiber_SetAccumulator(struct LMud_Fiber* self, LMud_Any value);

struct LMud_Frame* LMud_Fiber_PushFrame(struct LMud_Fiber* self, struct LMud_Function* function, struct LMud_Frame* lexical, LMud_Any* arguments, LMud_Size argument_count);
void               LMud_Fiber_PopFrame(struct LMud_Fiber* self);

void LMud_Fiber_Tick(struct LMud_Fiber* self);
