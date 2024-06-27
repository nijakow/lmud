
#include <lmud/lisp/gc.h>

#include "closure.h"

void LMud_Closure_Create(struct LMud_Closure* self, struct LMud_Function* function, struct LMud_Frame* lexical)
{
    self->function = function;
    LMud_FrameRef_Create(&self->lexical, lexical);
}

void LMud_Closure_Destroy(struct LMud_Closure* self)
{
    LMud_FrameRef_Destroy(&self->lexical);
}

void LMud_Closure_Mark(struct LMud_GC* gc, struct LMud_Closure* self)
{
    LMud_GC_MarkAny(gc, LMud_Any_FromPointer(self->function));
    LMud_GC_MarkFrame(gc, LMud_Closure_GetLexical(self));
}

LMud_Size LMud_Closure_CalculateSizeInBytes(struct LMud_Closure* self)
{
    (void) self;
    return sizeof(struct LMud_Closure);
}


struct LMud_Function* LMud_Closure_GetFunction(struct LMud_Closure* self)
{
    return self->function;
}

struct LMud_Frame* LMud_Closure_GetLexical(struct LMud_Closure* self)
{
    return LMud_FrameRef_GetFrame(&self->lexical);
}
