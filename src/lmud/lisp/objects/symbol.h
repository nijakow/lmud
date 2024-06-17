
#pragma once

#include <lmud/lisp/lisp.h>

struct LMud_Symbol
{
    struct LMud_Object   _;
    struct LMud_String*  name;
};

void LMud_Symbol_Create(struct LMud_Symbol* self, struct LMud_String* name);
void LMud_Symbol_Destroy(struct LMud_Symbol* self);
