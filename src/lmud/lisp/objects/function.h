
#pragma once

#include <lmud/lisp/base.h>


struct LMud_ArgInfo
{
    LMud_Size  stack_size;
    LMud_Size  register_count;
};

struct LMud_Function
{
    struct LMud_Object   _;

    struct LMud_ArgInfo  args;

    LMud_Any             bytecodes;
    LMud_Any             constants;
};

void LMud_Function_Create(struct LMud_Function* self, struct LMud_ArgInfo args, LMud_Any bytecodes, LMud_Any constants);
void LMud_Function_Destroy(struct LMud_Function* self);

LMud_Any LMud_Function_Bytecodes(struct LMud_Function* self);
LMud_Any LMud_Function_Constants(struct LMud_Function* self);
