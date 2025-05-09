
#include <lmud/lisp/gc.h>

#include "array.h"

void LMud_Array_Create_Overallocated(struct LMud_Array* self, LMud_Size size, LMud_Any fill)
{
    LMud_Size  index;

    self->size = size;
    self->data = self->payload;

    for (index = 0; index < size; index++)
    {
        self->data[index] = fill;
    }
}

void LMud_Array_Create_OverallocatedFromData(struct LMud_Array* self, LMud_Size size, LMud_Any* data)
{
    LMud_Size  index;

    self->size = size;
    self->data = self->payload;

    for (index = 0; index < size; index++)
    {
        self->data[index] = data[index];
    }
}

void LMud_Array_Destroy(struct LMud_Array* self)
{
    (void) self;
}

void LMud_Array_Mark(struct LMud_GC* gc, struct LMud_Array* self)
{
    LMud_Size  index;

    for (index = 0; index < LMud_Array_GetSize(self); index++)
    {
        LMud_GC_MarkAny(gc, self->data[index]);
    }
}

LMud_Size LMud_Array_CalculateSizeInBytes(struct LMud_Array* self)
{
    return sizeof(struct LMud_Array) + LMud_Array_GetSize(self) * sizeof(LMud_Any);
}

LMud_Size LMud_Array_GetSize(struct LMud_Array* self)
{
    return self->size;
}

LMud_Any* LMud_Array_GetData(struct LMud_Array* self)
{
    return self->data;
}

LMud_Any LMud_Array_Aref(struct LMud_Array* self, LMud_Size index, LMud_Any default_value)
{
    if (index >= LMud_Array_GetSize(self))
        return default_value;
    return self->data[index];
}

bool LMud_Array_Aset(struct LMud_Array* self, LMud_Size index, LMud_Any value)
{
    if (index >= LMud_Array_GetSize(self))
        return false;
    self->data[index] = value;
    return true;
}
