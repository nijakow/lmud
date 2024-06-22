
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

void LMud_Array_Destroy(struct LMud_Array* self)
{
    (void) self;
}
