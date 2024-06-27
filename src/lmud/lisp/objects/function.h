
#pragma once

#include <lmud/lisp/base.h>


struct LMud_ArgInfo
{
    LMud_Size  fixed_argument_count;
    LMud_Size  stack_size;
    LMud_Size  register_count;
    bool       lexicalized;
    bool       variadic;
};

struct LMud_Function
{
    struct LMud_ArgInfo  info;

    LMud_Any             bytecodes;
    LMud_Any             constants;
};

void LMud_Function_Create(struct LMud_Function* self, struct LMud_ArgInfo info, LMud_Any bytecodes, LMud_Any constants);
void LMud_Function_Destroy(struct LMud_Function* self);
void LMud_Function_Mark(struct LMud_GC* gc, struct LMud_Function* self);

LMud_Any LMud_Function_Bytecodes(struct LMud_Function* self);
LMud_Any LMud_Function_Constants(struct LMud_Function* self);

bool LMud_Function_IsLexicalized(struct LMud_Function* self);
