
#include <lmud/util/memory.h>

#include "objects.h"


void LMud_Types_Create(struct LMud_Types* self)
{
    self->array.base_size  = sizeof(struct LMud_Array);
    self->cons.base_size   = sizeof(struct LMud_Cons);
    self->string.base_size = sizeof(struct LMud_String);
    self->symbol.base_size = sizeof(struct LMud_Symbol);
}

void LMud_Types_Destroy(struct LMud_Types* self)
{
    (void) self;
}

bool LMud_Types_IsArray(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheck(&self->array, object);
}

bool LMud_Types_IsBytes(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheck(&self->bytes, object);
}

bool LMud_Types_IsCons(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheck(&self->cons, object);
}

bool LMud_Types_IsString(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheck(&self->string, object);
}

bool LMud_Types_IsSymbol(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheck(&self->symbol, object);
}


bool LMud_Objects_Create(struct LMud_Objects* self)
{
    self->objects = NULL;
    
    LMud_SymbolTable_Create(&self->symbols);
    LMud_Types_Create(&self->types);

    return true;
}

void LMud_Objects_Destroy(struct LMud_Objects* self)
{
    LMud_Types_Destroy(&self->types);
    LMud_SymbolTable_Destroy(&self->symbols);
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


struct LMud_Array*  LMud_Objects_MakeArray(struct LMud_Objects* self, LMud_Size size, LMud_Any fill)
{
    struct LMud_Array*  array;

    array = LMud_Objects_Allocate(self, &self->types.array, size * sizeof(LMud_Any));

    if (array != NULL)
    {
        LMud_Array_Create_Overallocated(array, size, fill);
    }

    return array;
}

struct LMud_Bytes* LMud_Objects_MakeBytes(struct LMud_Objects* self, LMud_Size size)
{
    struct LMud_Bytes*  bytes;

    bytes = LMud_Objects_Allocate(self, &self->types.bytes, size);

    if (bytes != NULL)
    {
        LMud_Bytes_Create_Overallocated(bytes, size);
    }

    return bytes;
}

struct LMud_Bytes* LMud_Objects_MakeBytes_FromData(struct LMud_Objects* self, LMud_Size size, const char* data)
{
    struct LMud_Bytes*  bytes;
    LMud_Size           index;

    bytes = LMud_Objects_MakeBytes(self, size);

    if (bytes != NULL)
    {
        for (index = 0; index < size; index++)
        {
            bytes->data[index] = data[index];
        }
    }

    return bytes;
}

struct LMud_Cons* LMud_Objects_Cons(struct LMud_Objects* self, LMud_Any car, LMud_Any cdr)
{
    struct LMud_Cons*  cons;

    cons = LMud_Objects_Allocate(self, &self->types.cons, 0);

    if (cons != NULL)
    {
        LMud_Cons_Create(cons, car, cdr);
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
    return LMud_SymbolTable_Intern(&self->symbols, self, name);
}

struct LMud_Symbol* LMud_Objects_Intern(struct LMud_Objects* self, const char* name)
{
    return LMud_Objects_PrimitiveIntern(self, name);
}
