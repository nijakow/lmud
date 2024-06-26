
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/lisp/runtime/frame.h>

#define LMud_Fiber_MAX_ACCUMULATORS 16

struct LMud_Fiber
{
    struct LMud_Lisp*      lisp;

    struct LMud_Frame*     top;
    char*                  stack;
    char*                  stack_roof;
    char*                  stack_pointer;

    LMud_Size              accumulator_count;
    LMud_Any               accumulator[LMud_Fiber_MAX_ACCUMULATORS];

    struct LMud_FrameList  floating_frames;
};

void LMud_Fiber_Create(struct LMud_Fiber* self, struct LMud_Lisp* lisp);
void LMud_Fiber_Destroy(struct LMud_Fiber* self);

bool LMud_Fiber_HasFrames(struct LMud_Fiber* self);

LMud_Any   LMud_Fiber_GetAccumulator(struct LMud_Fiber* self);
void       LMud_Fiber_SetAccumulator(struct LMud_Fiber* self, LMud_Any value);
void       LMud_Fiber_Values(struct LMud_Fiber* self, LMud_Any* values, LMud_Size count);
LMud_Size  LMud_Fiber_ValueCount(struct LMud_Fiber* self);
LMud_Any   LMud_Fiber_GetValue(struct LMud_Fiber* self, LMud_Size index);

struct LMud_Frame* LMud_Fiber_PushFrame(struct LMud_Fiber* self, struct LMud_Function* function, struct LMud_Frame* lexical, LMud_Any* arguments, LMud_Size argument_count);
void               LMud_Fiber_PopFrame(struct LMud_Fiber* self);

void LMud_Fiber_Enter(struct LMud_Fiber* self, LMud_Any function, LMud_Any* arguments, LMud_Size argument_count);
void LMud_Fiber_EnterThunk(struct LMud_Fiber* self, LMud_Any function);

void LMud_Fiber_PerformCall(struct LMud_Fiber* self, LMud_Any function, LMud_Size argument_count);
void LMud_Fiber_PerformReturn(struct LMud_Fiber* self);
void LMud_Fiber_PerformError(struct LMud_Fiber* self, const char* message);

void LMud_Fiber_Tick(struct LMud_Fiber* self);
