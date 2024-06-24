
#pragma once

#include <lmud/lisp/base.h>

struct LMud_Closure
{
    struct LMud_Function*  function;
    struct LMud_Frame*     lexical;
};

void LMud_Closure_Create(struct LMud_Closure* self, struct LMud_Function* function, struct LMud_Frame* lexical);
void LMud_Closure_Destroy(struct LMud_Closure* self);
