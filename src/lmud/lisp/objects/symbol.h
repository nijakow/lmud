
#pragma once

#include <lmud/lisp/lisp.h>

struct LMud_Symbol
{
    struct LMud_Object   _;

    struct LMud_Symbol** prev;
    struct LMud_Symbol*  next;

    LMud_Any             name;

    LMud_Any             value;
    LMud_Any             function;
};

void LMud_Symbol_Create(struct LMud_Symbol* self, struct LMud_Symbol** list, LMud_Any name);
void LMud_Symbol_Destroy(struct LMud_Symbol* self);

void LMud_Symbol_Unlink(struct LMud_Symbol* self);
void LMud_Symbol_Link(struct LMud_Symbol* self, struct LMud_Symbol** list);
