
#pragma once

#include <lmud/lisp/base.h>

struct LMud_StackFrame
{
    struct LMud_StackFrame**  slot;
    struct LMud_Frame*        frame;
};

void LMud_StackFrame_Create(struct LMud_StackFrame* self, struct LMud_Frame* frame, struct LMud_StackFrame** slot);
void LMud_StackFrame_Destroy(struct LMud_StackFrame* self);
void LMud_StackFrame_Mark(struct LMud_GC* gc, struct LMud_StackFrame* self);
LMud_Size LMud_StackFrame_CalculateSizeInBytes(struct LMud_StackFrame* self);
