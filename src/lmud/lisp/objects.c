
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


struct LMud_Cons* LMud_Objects_Cons(struct LMud_Objects* self, LMud_Any car, LMud_Any cdr)
{
    struct LMud_Cons*  cons;

    cons = LMud_Objects_Allocate(self, &self->types.cons, 0);

    if (cons != NULL)
    {
        cons->car = car;
        cons->cdr = cdr;
    }

    return cons;
}

struct LMud_String* LMud_Objects_String(struct LMud_Objects* self, const char* text)
{
    struct LMud_String*  string;
    LMud_Size            length;
    LMud_Size            index;

    length = LMud_CStr_Length(text);

    string = LMud_Objects_Allocate(self, &self->types.string, length + 1);

    if (string != NULL)
    {
        string->chars = string->payload;

        for (index = 0; index < length; index++)
        {
            string->chars[index] = text[index];
        }

        string->chars[index] = '\0';
    }

    return string;
}


struct LMud_Symbol* LMud_Objects_PrimitiveIntern(struct LMud_Objects* self, const char* name)
{
    (void) self;
    (void) name;

    return NULL;
}

struct LMud_Symbol* LMud_Objects_Intern(struct LMud_Objects* self, const char* name)
{
    return LMud_Objects_PrimitiveIntern(self, name);
}
