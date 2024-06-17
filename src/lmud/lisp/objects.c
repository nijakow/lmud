
#include "objects.h"


void LMud_Objects_Create(struct LMud_Objects* self)
{
    self->objects = NULL;
}

void LMud_Objects_Destroy(struct LMud_Objects* self)
{
    (void) self;
}
