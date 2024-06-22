
#pragma once

#include <lmud/lisp/base.h>

struct LMud_Array
{
    struct LMud_Object  _;

    LMud_Size           size;
    LMud_Any*           data;
    LMud_Any            payload[];
};

void LMud_Array_Create_Overallocated(struct LMud_Array* self, LMud_Size size, LMud_Any fill);
void LMud_Array_Destroy(struct LMud_Array* self);

LMud_Size LMud_Array_GetSize(struct LMud_Array* self);
LMud_Any  LMud_Array_Aref(struct LMud_Array* self, LMud_Size index, LMud_Any default_value);
