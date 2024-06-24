
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
