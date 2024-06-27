
#pragma once

#include <lmud/lisp/base.h>

#include <lmud/lisp/runtime/frame.h>


struct LMud_Closure
{
    struct LMud_Function*  function;
    struct LMud_FrameRef   lexical;
};

void LMud_Closure_Create(struct LMud_Closure* self, struct LMud_Function* function, struct LMud_Frame* lexical);
void LMud_Closure_Destroy(struct LMud_Closure* self);
void LMud_Closure_Mark(struct LMud_GC* gc, struct LMud_Closure* self);
LMud_Size LMud_Closure_CalculateSizeInBytes(struct LMud_Closure* self);

struct LMud_Function* LMud_Closure_GetFunction(struct LMud_Closure* self);
struct LMud_Frame*    LMud_Closure_GetLexical(struct LMud_Closure* self);
