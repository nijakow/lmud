
#pragma once

#include <lmud/lisp/base.h>

struct LMud_Bytes
{
    LMud_Size           size;
    char*               data;
    char                payload[];
};

void LMud_Bytes_Create_Overallocated(struct LMud_Bytes* self, LMud_Size size);
void LMud_Bytes_Destroy(struct LMud_Bytes* self);
void LMud_Bytes_Mark(struct LMud_GC* gc, struct LMud_Bytes* self);
LMud_Size LMud_Bytes_CalculateSizeInBytes(struct LMud_Bytes* self);

LMud_Size LMud_Bytes_GetSize(struct LMud_Bytes* self);
char*     LMud_Bytes_GetData(struct LMud_Bytes* self);
char      LMud_Bytes_At(struct LMud_Bytes* self, LMud_Size index);
LMud_Any  LMud_Bytes_Aref(struct LMud_Bytes* self, LMud_Size index);
bool      LMud_Bytes_Aset(struct LMud_Bytes* self, LMud_Size index, LMud_Any value);
