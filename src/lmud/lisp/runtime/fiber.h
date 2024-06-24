
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/lisp/runtime/frame.h>

struct LMud_Fiber
{
    struct LMud_Frame*  top;
    char*               stack;
    char*               stack_roof;
    char*               stack_pointer;
};

void LMud_Fiber_Create(struct LMud_Fiber* self);
void LMud_Fiber_Destroy(struct LMud_Fiber* self);

struct LMud_Frame* LMud_Fiber_PushFrame(struct LMud_Fiber* self, struct LMud_Function* function, struct LMud_Frame* lexical, LMud_Any* arguments, LMud_Size argument_count);
void               LMud_Fiber_PopFrame(struct LMud_Fiber* self);
