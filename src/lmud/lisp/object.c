
#include <lmud/lisp/objects.h>

#include "object.h"


bool LMud_Object_Create(struct LMud_Object* self, struct LMud_Objects* objects, struct LMud_Type* type)
{
    (void) objects;

    self->next       = objects->objects;
    objects->objects = self;

    self->type = type;

    return true;
}

void LMud_Object_Destroy(struct LMud_Object* self)
{
    (void) self;
}


bool LMud_Type_TypeCheck(struct LMud_Type* self, struct LMud_Object* object)
{
    return object->type == self;
}
