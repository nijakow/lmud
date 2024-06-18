
#include <lmud/util/memory.h>

#include "objects.h"


void LMud_Types_Create(struct LMud_Types* self)
{
    (void) self;
}

void LMud_Types_Destroy(struct LMud_Types* self)
{
    (void) self;
}


void LMud_Objects_Create(struct LMud_Objects* self)
{
    self->objects = NULL;

    LMud_Types_Create(&self->types);
}

void LMud_Objects_Destroy(struct LMud_Objects* self)
{
    LMud_Types_Destroy(&self->types);
}


void* LMud_Objects_Allocate(struct LMud_Objects* self, struct LMud_Type* type, LMud_Size extra)
{
    struct LMud_Object*  object;

    object = LMud_Alloc(type->base_size + extra);

    if (object != NULL)
    {
        LMud_Object_Create(object, self, type);
    }

    return object;
}
