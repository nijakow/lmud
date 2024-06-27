
#include <lmud/lisp/lisp.h>
#include <lmud/util/memory.h>

#include "objects.h"


#define LMud_Types_CreateType(struct_name, name) \
    { \
        self->name.base_size  = sizeof(struct struct_name); \
        self->name.size_func  = (LMud_SizeFunc) struct_name##_CalculateSizeInBytes; \
        self->name.marker     = (LMud_MarkFunc) struct_name##_Mark; \
        self->name.destructor = (LMud_Destructor) struct_name##_Destroy; \
    }


void LMud_Types_Create(struct LMud_Types* self)
{
    LMud_Types_CreateType(LMud_Array, array);
    LMud_Types_CreateType(LMud_Builtin, builtin);
    LMud_Types_CreateType(LMud_Bytes, bytes);
    LMud_Types_CreateType(LMud_Closure, closure);
    LMud_Types_CreateType(LMud_Cons, cons);
    LMud_Types_CreateType(LMud_Custom, custom);
    LMud_Types_CreateType(LMud_Function, function);
    LMud_Types_CreateType(LMud_Package, package);
    LMud_Types_CreateType(LMud_Ratio, ratio);
    LMud_Types_CreateType(LMud_String, string);
    LMud_Types_CreateType(LMud_Symbol, symbol);
}

void LMud_Types_Destroy(struct LMud_Types* self)
{
    (void) self;
}

bool LMud_Types_IsArray(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheckObject(&self->array, object);
}

bool LMud_Types_IsBuiltin(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheckObject(&self->builtin, object);
}

bool LMud_Types_IsBytes(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheckObject(&self->bytes, object);
}

bool LMud_Types_IsClosure(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheckObject(&self->closure, object);
}

bool LMud_Types_IsCons(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheckObject(&self->cons, object);
}

bool LMud_Types_IsCustom(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheckObject(&self->custom, object);
}

bool LMud_Types_IsFunction(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheckObject(&self->function, object);
}

bool LMud_Types_IsPackage(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheckObject(&self->package, object);
}

bool LMud_Types_IsRatio(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheckObject(&self->ratio, object);
}

bool LMud_Types_IsString(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheckObject(&self->string, object);
}

bool LMud_Types_IsSymbol(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheckObject(&self->symbol, object);
}


bool LMud_Objects_Create(struct LMud_Objects* self, struct LMud_Lisp* lisp)
{
    self->lisp     = lisp;

    self->objects  = NULL;
    self->packages = NULL;

    LMud_Types_Create(&self->types);

    return true;
}

void LMud_Objects_Destroy(struct LMud_Objects* self)
{
    LMud_Types_Destroy(&self->types);
}


struct LMud_Lisp* LMud_Objects_GetLisp(struct LMud_Objects* self)
{
    return self->lisp;
}


void* LMud_Objects_Allocate(struct LMud_Objects* self, struct LMud_Type* type, LMud_Size extra)
{
    struct LMud_Header*  object;

    object = LMud_Alloc(sizeof(struct LMud_Header) + type->base_size + extra);

    if (object != NULL)
    {
        LMud_Header_Create(object, self, type);
    }

    return LMud_Header_ToObject(object);
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

struct LMud_Array*  LMud_Objects_MakeArray_FromData(struct LMud_Objects* self, LMud_Size size, LMud_Any* data)
{
    struct LMud_Array*  array;

    array = LMud_Objects_Allocate(self, &self->types.array, size * sizeof(LMud_Any));

    if (array != NULL)
    {
        LMud_Array_Create_OverallocatedFromData(array, size, data);
    }

    return array;
}

struct LMud_Builtin* LMud_Objects_Builtin(struct LMud_Objects* self, const char* name, LMud_BuiltinFunction function)
{
    struct LMud_Builtin*  builtin;

    builtin = LMud_Objects_Allocate(self, &self->types.builtin, 0);

    if (builtin != NULL)
    {
        LMud_Builtin_Create(builtin, name, function);
    }

    return builtin;
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

struct LMud_Closure* LMud_Objects_Closure(struct LMud_Objects* self, struct LMud_Function* function, struct LMud_Frame* lexical)
{
    struct LMud_Closure*  closure;

    closure = LMud_Objects_Allocate(self, &self->types.closure, 0);

    if (closure != NULL)
    {
        LMud_Closure_Create(closure, function, lexical);
    }

    return closure;
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

struct LMud_Custom* LMud_Objects_Custom(struct LMud_Objects* self, LMud_Any meta, LMud_Any* slots, LMud_Size size)
{
    struct LMud_Custom*  custom;

    custom = LMud_Objects_Allocate(self, &self->types.custom, size * sizeof(LMud_Any));

    if (custom != NULL)
    {
        LMud_Custom_Create(custom, meta, slots, size);
    }

    return custom;
}

struct LMud_Function* LMud_Objects_Function(struct LMud_Objects* self, struct LMud_ArgInfo info, LMud_Any bytecodes, LMud_Any constants)
{
    struct LMud_Function*  function;

    function = LMud_Objects_Allocate(self, &self->types.function, 0);

    if (function != NULL)
    {
        LMud_Function_Create(function, info, bytecodes, constants);
    }

    return function;
}

struct LMud_Package* LMud_Objects_Package(struct LMud_Objects* self, LMud_Any name)
{
    struct LMud_Package*  package;

    for (package = self->packages; package != NULL; package = package->next)
    {
        if (LMud_Lisp_IsString(self->lisp, package->name) && LMud_CStr_Equals(LMud_String_Chars(LMud_Any_AsPointer(package->name)), LMud_String_Chars(LMud_Any_AsPointer(name))))
        {
            return package;
        }
    }

    package = LMud_Objects_Allocate(self, &self->types.package, 0);

    if (package != NULL)
    {
        LMud_Package_Create(package, name);
        LMud_Package_Link(package, &self->packages);
    }

    return package;
}

struct LMud_Ratio* LMud_Objects_Ratio(struct LMud_Objects* self, LMud_Any numerator, LMud_Any denominator)
{
    struct LMud_Ratio*  ratio;

    ratio = LMud_Objects_Allocate(self, &self->types.ratio, 0);

    if (ratio != NULL)
    {
        LMud_Ratio_Create(ratio, numerator, denominator);
    }

    return ratio;
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


struct LMud_Symbol* LMud_Objects_PrimitiveIntern(struct LMud_Objects* self, struct LMud_Package* package, const char* name)
{
    return LMud_SymbolTable_Intern(LMud_Package_GetSymbolTable(package), self, name, LMud_Any_FromPointer(package));
}

struct LMud_Symbol* LMud_Objects_Intern(struct LMud_Objects* self, struct LMud_Package* package, const char* name)
{
    return LMud_Objects_PrimitiveIntern(self, package, name);
}

struct LMud_Symbol* LMud_Objects_Gensym(struct LMud_Objects* self)
{
    struct LMud_Symbol*  symbol;

    symbol = LMud_Objects_Allocate(self, &self->types.symbol, 0);

    if (symbol != NULL)
    {
        LMud_Symbol_Create(
            symbol,
            NULL,
            LMud_Lisp_Nil(self->lisp),
            LMud_Lisp_Nil(self->lisp),
            LMud_Lisp_Nil(self->lisp),
            LMud_Lisp_Nil(self->lisp),
            LMud_Lisp_Nil(self->lisp),
            LMud_Lisp_Nil(self->lisp)
        );

        LMud_Symbol_MakeGensym(symbol);
    }

    return symbol;
}
