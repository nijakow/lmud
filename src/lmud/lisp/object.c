
#include "object.h"


bool LMud_Object_Create(struct LMud_Object* self, struct LMud_Objects* objects, struct LMud_Type* type)
{
    (void) objects;

    self->header.type = type;

    return true;
}

void LMud_Object_Destroy(struct LMud_Object* self)
{
    (void) self;
}
