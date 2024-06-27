
#include <lmud/lisp/gc.h>

#include "bytes.h"

void LMud_Bytes_Create_Overallocated(struct LMud_Bytes* self, LMud_Size size)
{
    self->size = size;
    self->data = self->payload;
}

void LMud_Bytes_Destroy(struct LMud_Bytes* self)
{
    (void) self;
}

void LMud_Bytes_Mark(struct LMud_GC* gc, struct LMud_Bytes* self)
{
    (void) gc;
    (void) self;
}

LMud_Size LMud_Bytes_CalculateSizeInBytes(struct LMud_Bytes* self)
{
    return sizeof(struct LMud_Bytes) + LMud_Bytes_GetSize(self);
}

LMud_Size LMud_Bytes_GetSize(struct LMud_Bytes* self)
{
    return self->size;
}

char* LMud_Bytes_GetData(struct LMud_Bytes* self)
{
    return self->data;
}

char LMud_Bytes_At(struct LMud_Bytes* self, LMud_Size index)
{
    return self->data[index];
}

LMud_Any LMud_Bytes_Aref(struct LMud_Bytes* self, LMud_Size index)
{
    return LMud_Any_FromInteger(LMud_Bytes_At(self, index));
}

bool LMud_Bytes_Aset(struct LMud_Bytes* self, LMud_Size index, LMud_Any value)
{
    if (index >= LMud_Bytes_GetSize(self))
        return false;
    self->data[index] = LMud_Any_AsInteger(value);
    return true;
}
