
#include <lmud/util/stringbuilder.h>

#include "lisp.h"


bool LMud_Constants_Create(struct LMud_Constants* self, struct LMud_Lisp* lisp)
{
    self->nil      = LMud_Lisp_Intern(lisp, "NIL");
    self->t        = LMud_Lisp_Intern(lisp, "T");
    self->quote    = LMud_Lisp_Intern(lisp, "QUOTE");
    self->function = LMud_Lisp_Intern(lisp, "FUNCTION");

    return true;
}

void LMud_Constants_Destroy(struct LMud_Constants* self)
{
    (void) self;
}



bool LMud_Lisp_Create(struct LMud_Lisp* self)
{
    return LMud_Objects_Create(&self->objects)
        && LMud_Constants_Create(&self->constants, self);
}

void LMud_Lisp_Destroy(struct LMud_Lisp* self)
{
    LMud_Constants_Destroy(&self->constants);
    LMud_Objects_Destroy(&self->objects);
}


struct LMud_Types* LMud_Lisp_Types(struct LMud_Lisp* self)
{
    return &self->objects.types;
}


bool LMud_Lisp_IsArrayPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsArray(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsConsPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsCons(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsStringPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsString(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsSymbolPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsSymbol(LMud_Lisp_Types(self), object);
}


bool LMud_Lisp_IsArray(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsArrayPointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsCons(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsConsPointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsString(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsStringPointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsSymbol(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsSymbolPointer(self, LMud_Any_AsPointer(value));
}


bool LMud_Lisp_IsNil(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_Eq(LMud_Lisp_Nil(self), value);
}


LMud_Any LMud_Lisp_Nil(struct LMud_Lisp* self)
{
    return self->constants.nil;
}

LMud_Any LMud_Lisp_MakeArray(struct LMud_Lisp* self, LMud_Size size, LMud_Any fill)
{
    return LMud_Any_FromPointer(LMud_Objects_MakeArray(&self->objects, size, fill));
}

LMud_Any LMud_Lisp_Cons(struct LMud_Lisp* self, LMud_Any car, LMud_Any cdr)
{
    return LMud_Any_FromPointer(LMud_Objects_Cons(&self->objects, car, cdr));
}

LMud_Any LMud_Lisp_String(struct LMud_Lisp* self, const char* text)
{
    return LMud_Any_FromPointer(LMud_Objects_String(&self->objects, text));
}

LMud_Any LMud_Lisp_Intern(struct LMud_Lisp* self, const char* name)
{
    return LMud_Any_FromPointer(LMud_Objects_Intern(&self->objects, name));
}

LMud_Any LMud_Lisp_InternUpcase(struct LMud_Lisp* self, const char* name)
{
    struct LMud_StringBuilder  builder;
    LMud_Any                   result;
    LMud_Size                  index;

    LMud_StringBuilder_Create(&builder);

    for (index = 0; name[index] != '\0'; index++)
        LMud_StringBuilder_AppendChar(&builder, toupper(name[index]));

    result = LMud_Lisp_Intern(self, LMud_StringBuilder_GetStatic(&builder));

    LMud_StringBuilder_Destroy(&builder);

    return result;
}


LMud_Any LMud_Lisp_Car(struct LMud_Lisp* self, LMud_Any value)
{
    if (!LMud_Lisp_IsCons(self, value))
        return value;
    return ((struct LMud_Cons*) LMud_Any_AsPointer(value))->car;
}

LMud_Any LMud_Lisp_Cdr(struct LMud_Lisp* self, LMud_Any value)
{
    if (!LMud_Lisp_IsCons(self, value))
        return value;
    return ((struct LMud_Cons*) LMud_Any_AsPointer(value))->cdr;
}

bool LMud_Lisp_Nth(struct LMud_Lisp* self, LMud_Any value, LMud_Size index, LMud_Any* result)
{
    while (index > 0)
    {
        if (!LMud_Lisp_IsCons(self, value))
            return false;
        value = LMud_Lisp_Cdr(self, value);
        index--;
    }

    *result = value;
    return true;
}


LMud_Any LMud_Lisp_Quote(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Lisp_Cons(self, self->constants.quote, LMud_Lisp_Cons(self, value, LMud_Lisp_Nil(self)));
}

LMud_Any LMud_Lisp_QuoteFunction(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Lisp_Cons(self, self->constants.function, LMud_Lisp_Cons(self, value, LMud_Lisp_Nil(self)));
}
