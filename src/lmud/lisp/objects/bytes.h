
#pragma once

#include <lmud/lisp/base.h>

struct LMud_Bytes
{
    struct LMud_Object  _;

    LMud_Size           size;
    char*               data;
    char                payload[];
};

void LMud_Bytes_Create_Overallocated(struct LMud_Bytes* self, LMud_Size size);
void LMud_Bytes_Destroy(struct LMud_Bytes* self);

LMud_Size LMud_Bytes_GetSize(struct LMud_Bytes* self);
char      LMud_Bytes_At(struct LMud_Bytes* self, LMud_Size index);
LMud_Any  LMud_Bytes_Aref(struct LMud_Bytes* self, LMud_Size index);
