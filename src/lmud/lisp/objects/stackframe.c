
#include "stackframe.h"

void LMud_StackFrame_Create(struct LMud_StackFrame* self, struct LMud_Frame* frame, struct LMud_StackFrame** slot)
{
    self->frame = frame;
    self->slot  = slot;

    if (slot != NULL)
    {
        *slot = self;
    }
}

void LMud_StackFrame_Destroy(struct LMud_StackFrame* self)
{
    if (self->slot != NULL)
    {
        *self->slot = NULL;
    }
}

void LMud_StackFrame_Mark(struct LMud_GC* gc, struct LMud_StackFrame* self)
{
    (void) self;
    (void) gc;
}

LMud_Size LMud_StackFrame_CalculateSizeInBytes(struct LMud_StackFrame* self)
{
    (void) self;
    return sizeof(struct LMud_StackFrame);
}
