
#include <lmud/util/memory.h>

#include "objects.h"


void LMud_Types_Create(struct LMud_Types* self)
{
    self->cons.base_size   = sizeof(struct LMud_Cons);
    self->string.base_size = sizeof(struct LMud_String);
    self->symbol.base_size = sizeof(struct LMud_Symbol);
}

void LMud_Types_Destroy(struct LMud_Types* self)
{
    (void) self;
}


bool LMud_Objects_Create(struct LMud_Objects* self)
{
    self->objects = NULL;
    self->symbols = NULL;

    LMud_Types_Create(&self->types);

    return true;
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
    struct LMud_Symbol*  symbol;

    for (symbol = self->symbols; symbol != NULL; symbol = symbol->next)
    {
        if (LMud_String_Equals(LMud_Any_AsPointer(symbol->name), name))
        {
            return symbol;
        }
    }

    symbol = LMud_Objects_Allocate(self, &self->types.symbol, 0);

    if (symbol != NULL)
    {
        LMud_Symbol_Create(symbol, &self->symbols, LMud_Any_FromPointer(LMud_Objects_String(self, name)));
    }

    return symbol;
}

struct LMud_Symbol* LMud_Objects_Intern(struct LMud_Objects* self, const char* name)
{
    return LMud_Objects_PrimitiveIntern(self, name);
}
