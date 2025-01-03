
#include <lmud/lisp/lisp.h>
#include <lmud/util/memory.h>

#include "objects.h"


#define LMud_Types_CreateType(struct_name, slot) \
    { \
        self->slot.name       = ""#struct_name; \
        self->slot.base_size  = sizeof(struct struct_name); \
        self->slot.size_func  = (LMud_SizeFunc) struct_name##_CalculateSizeInBytes; \
        self->slot.marker     = (LMud_MarkFunc) struct_name##_Mark; \
        self->slot.destructor = (LMud_Destructor) struct_name##_Destroy; \
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
    LMud_Types_CreateType(LMud_Port, port);
    LMud_Types_CreateType(LMud_Process, process);
    LMud_Types_CreateType(LMud_Ratio, ratio);
    LMud_Types_CreateType(LMud_StackFrame, stack_frame);
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

bool LMud_Types_IsPort(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheckObject(&self->port, object);
}

bool LMud_Types_IsProcess(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheckObject(&self->process, object);
}

bool LMud_Types_IsRatio(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheckObject(&self->ratio, object);
}

bool LMud_Types_IsStackFrame(struct LMud_Types* self, void* object)
{
    return LMud_Type_TypeCheckObject(&self->stack_frame, object);
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

    self->bytes_allocated          = 0;
    self->bytes_allocated_since_gc = 0;
    self->objects_allocated        = 0;

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


LMud_Size LMud_Objects_GetGcAllocationCounter(struct LMud_Objects* self)
{
    return self->bytes_allocated_since_gc;
}

void LMud_Objects_ClearGcAllocationCounter(struct LMud_Objects* self)
{
    self->bytes_allocated_since_gc = 0;
}


void* LMud_Objects_Allocate(struct LMud_Objects* self, struct LMud_Type* type, LMud_Size extra)
{
    struct LMud_Header*  object;

    object = LMud_Alloc(sizeof(struct LMud_Header) + type->base_size + extra);

    if (object != NULL)
    {
        LMud_Header_Create(object, self, type);
        self->objects_allocated++;
    }

    return LMud_Header_ToObject(object);
}

static void LMud_Objects_AfterAllocate(struct LMud_Objects* self, void* object)
{
    struct LMud_Header*  header;
    LMud_Size            size;

    header = LMud_ToHeader(object);
    size   = header->type->size_func(object) + sizeof(struct LMud_Header);

    self->bytes_allocated          += size;
    self->bytes_allocated_since_gc += size;
    
    LMud_Debugf(self->lisp->mud, LMud_LogLevel_ALL, "Allocated %zu bytes for %s (%p), now %zu bytes allocated", size, header->type->name, object, self->bytes_allocated);
}

struct LMud_Array*  LMud_Objects_MakeArray(struct LMud_Objects* self, LMud_Size size, LMud_Any fill)
{
    struct LMud_Array*  array;

    array = LMud_Objects_Allocate(self, &self->types.array, size * sizeof(LMud_Any));

    if (array != NULL)
    {
        LMud_Array_Create_Overallocated(array, size, fill);
        LMud_Objects_AfterAllocate(self, array);
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
        LMud_Objects_AfterAllocate(self, array);
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
        LMud_Objects_AfterAllocate(self, builtin);
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
        LMud_Objects_AfterAllocate(self, bytes);
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
        LMud_Objects_AfterAllocate(self, closure);
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
        LMud_Objects_AfterAllocate(self, cons);
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
        LMud_Objects_AfterAllocate(self, custom);
    }

    return custom;
}

struct LMud_Function* LMud_Objects_Function(struct LMud_Objects* self, struct LMud_ArgInfo info, LMud_Any bytecodes, LMud_Any constants, LMud_Any source_code)
{
    struct LMud_Function*  function;

    function = LMud_Objects_Allocate(self, &self->types.function, 0);

    if (function != NULL)
    {
        LMud_Function_Create(function, info, bytecodes, constants, source_code);
        LMud_Objects_AfterAllocate(self, function);
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
        LMud_Objects_AfterAllocate(self, package);
    }

    return package;
}

struct LMud_Port* LMud_Objects_Port(struct LMud_Objects* self, struct LMud_Connection* connection)
{
    struct LMud_Port*  port;

    port = LMud_Objects_Allocate(self, &self->types.port, 0);

    if (port != NULL)
    {
        LMud_Port_Create(port, connection);
        LMud_Objects_AfterAllocate(self, port);
    }

    return port;
}

struct LMud_Process* LMud_Objects_Process(struct LMud_Objects* self, struct LMud_Fiber* fiber, struct LMud_Process** slot)
{
    struct LMud_Process*  process;

    process = LMud_Objects_Allocate(self, &self->types.process, 0);

    if (process != NULL)
    {
        LMud_Process_Create(process, fiber, slot);
        LMud_Objects_AfterAllocate(self, process);
    }

    return process;
}

struct LMud_Ratio* LMud_Objects_Ratio(struct LMud_Objects* self, LMud_Any numerator, LMud_Any denominator)
{
    struct LMud_Ratio*  ratio;

    ratio = LMud_Objects_Allocate(self, &self->types.ratio, 0);

    if (ratio != NULL)
    {
        LMud_Ratio_Create(ratio, numerator, denominator);
        LMud_Objects_AfterAllocate(self, ratio);
    }

    return ratio;
}

struct LMud_StackFrame* LMud_Objects_StackFrame(struct LMud_Objects* self, struct LMud_Frame* frame, struct LMud_StackFrame** slot)
{
    struct LMud_StackFrame*  stack_frame;

    stack_frame = LMud_Objects_Allocate(self, &self->types.stack_frame, 0);

    if (stack_frame != NULL)
    {
        LMud_StackFrame_Create(stack_frame, frame, slot);
        LMud_Objects_AfterAllocate(self, stack_frame);
    }

    return stack_frame;
}

struct LMud_String* LMud_Objects_String(struct LMud_Objects* self, const char* text)
{
    struct LMud_String*  string;
    LMud_Size            length;

    length = LMud_CStr_Length(text);

    string = LMud_Objects_Allocate(self, &self->types.string, length + 1);

    if (string != NULL)
    {
        LMud_String_Create_Overallocated(string, text);
        LMud_Objects_AfterAllocate(self, string);
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

        LMud_Objects_AfterAllocate(self, symbol);
    }

    return symbol;
}
