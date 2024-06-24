
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/lisp/objects/function.h>

struct LMud_Frame
{
    struct LMud_Frame*     previous;
    struct LMud_Frame*     lexical;
    LMud_Any*              arguments_base;
    LMud_Any*              arguments_top;
    struct LMud_Function*  function;
    LMud_Size              ip;
    LMud_Size              sp;
    LMud_Any               payload[];
};

void LMud_Frame_Create(struct LMud_Frame*    self,
                       struct LMud_Frame*    previous,
                       struct LMud_Frame*    lexical,
                       struct LMud_Function* function,
                       LMud_Any*             arguments_base,
                       LMud_Size             arguments_count);

void LMud_Frame_Destroy(struct LMud_Frame* self);
