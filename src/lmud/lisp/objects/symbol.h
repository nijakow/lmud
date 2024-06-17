
#pragma once

#include <lmud/lisp/lisp.h>

struct LMud_Symbol
{
    struct LMud_Object   _;

    LMud_Any             name;

    LMud_Any             value;
    LMud_Any             function;
};

void LMud_Symbol_Create(struct LMud_Symbol* self, LMud_Any name);
void LMud_Symbol_Destroy(struct LMud_Symbol* self);
