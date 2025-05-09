
#include <lmud/lisp/builtins.h>
#include <lmud/lisp/compiler/compiler.h>
#include <lmud/lisp/gc.h>
#include <lmud/lisp/io.h>
#include <lmud/util/stringbuilder.h>
#include <lmud/util/vt100.h>

#include "lisp.h"


bool LMud_Constants_Create(struct LMud_Constants* self, struct LMud_Lisp* lisp)
{
    self->default_package = LMud_Lisp_Package(lisp, LMud_Lisp_String(lisp, "LISP"));
    self->keyword_package = LMud_Lisp_Package(lisp, LMud_Lisp_String(lisp, "KEYWORD"));

    /*
     * We have a bit of a chicken-and-egg problem here. We need to create the
     * NIL and T symbols, but we need to have the lisp object to initialize
     * them. So we have to do this manually.
     */
    self->nil      = LMud_Lisp_Intern(lisp, "NIL");
    LMud_Symbol_SetValue(LMud_Any_AsPointer(self->nil), self->nil, true);
    LMud_Symbol_SetFunction(LMud_Any_AsPointer(self->nil), self->nil, true);
    LMud_Symbol_SetMacro(LMud_Any_AsPointer(self->nil), self->nil, true);
    LMud_Symbol_SetPlist(LMud_Any_AsPointer(self->nil), self->nil, true);
    self->t        = LMud_Lisp_Intern(lisp, "T");
    LMud_Symbol_MakeConstant(LMud_Any_AsPointer(self->nil), true);
    LMud_Symbol_MakeConstant(LMud_Any_AsPointer(self->t), true);

    self->quote    = LMud_Lisp_Intern(lisp, "QUOTE");
    self->function = LMud_Lisp_Intern(lisp, "FUNCTION");

    self->custom_dispatcher_function = self->nil;
    self->new_connection_function    = self->nil;

    LMud_Lisp_InstallBuiltins(lisp);

    return true;
}

void LMud_Constants_Destroy(struct LMud_Constants* self)
{
    (void) self;
}

void LMud_Constants_Mark(struct LMud_GC* gc, struct LMud_Constants* self)
{
    LMud_GC_MarkAny(gc, self->default_package);
    LMud_GC_MarkAny(gc, self->keyword_package);
    LMud_GC_MarkAny(gc, self->nil);
    LMud_GC_MarkAny(gc, self->t);
    LMud_GC_MarkAny(gc, self->quote);
    LMud_GC_MarkAny(gc, self->function);
    LMud_GC_MarkAny(gc, self->custom_dispatcher_function);
    LMud_GC_MarkAny(gc, self->new_connection_function);
}



bool LMud_Lisp_Create(struct LMud_Lisp* self, struct LMud* mud)
{
    self->mud = mud;

    LMud_InputStream_CreateFromFile(&self->standard_input, stdin);
    LMud_FrameList_Create(&self->floating_frames);

    return LMud_Objects_Create(&self->objects, self)
        && LMud_Constants_Create(&self->constants, self)
        && LMud_Scheduler_Create(&self->scheduler, self);
}

void LMud_Lisp_Destroy(struct LMud_Lisp* self)
{
    LMud_FrameList_Destroy(&self->floating_frames);
    LMud_Scheduler_Destroy(&self->scheduler);
    LMud_Constants_Destroy(&self->constants);
    LMud_Objects_Destroy(&self->objects);
    LMud_InputStream_Destroy(&self->standard_input);
}

void LMud_Lisp_Mark(struct LMud_GC* gc, struct LMud_Lisp* self)
{
    LMud_Constants_Mark(gc, &self->constants);
    LMud_Scheduler_Mark(gc, &self->scheduler);
}


struct LMud_Objects* LMud_Lisp_Objects(struct LMud_Lisp* self)
{
    return &self->objects;
}

struct LMud_Scheduler* LMud_Lisp_Scheduler(struct LMud_Lisp* self)
{
    return &self->scheduler;
}

struct LMud_Types* LMud_Lisp_Types(struct LMud_Lisp* self)
{
    return &self->objects.types;
}


void LMud_Lisp_FetchMemoryReport(struct LMud_Lisp* self, struct LMud_MemoryReport* report)
{
    report->objects_allocated = self->objects.objects_allocated;
}


bool LMud_Lisp_IsArrayPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsArray(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsBuiltinPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsBuiltin(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsBytesPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsBytes(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsClosurePointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsClosure(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsConsPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsCons(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsCustomPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsCustom(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsFunctionPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsFunction(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsPackagePointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsPackage(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsPortPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsPort(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsProcessPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsProcess(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsRatioPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsRatio(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsStackFramePointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsStackFrame(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsStringPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsString(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsSymbolPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsSymbol(LMud_Lisp_Types(self), object);
}


bool LMud_Lisp_IsInteger(struct LMud_Lisp* self, LMud_Any value)
{
    (void) self;
    return LMud_Any_IsInteger(value);
}

bool LMud_Lisp_IsCharacter(struct LMud_Lisp* self, LMud_Any value)
{
    (void) self;
    return LMud_Any_IsCharacter(value);
}

bool LMud_Lisp_IsArray(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsArrayPointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsBuiltin(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsBuiltinPointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsBytes(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsBytesPointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsClosure(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsClosurePointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsCons(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsConsPointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsCustom(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsCustomPointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsFunction(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsFunctionPointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsPackage(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsPackagePointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsPort(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsPortPointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsProcess(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsProcessPointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsRatio(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsRatioPointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsStackFrame(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsStackFramePointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsString(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsStringPointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsSymbol(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsSymbolPointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsGensym(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Lisp_IsSymbol(self, value) && LMud_Symbol_IsGensym(LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsNumber(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Lisp_IsInteger(self, value) || LMud_Lisp_IsRatio(self, value);
}


bool LMud_Lisp_IsNil(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_Eq(LMud_Lisp_Nil(self), value);
}


LMud_Any LMud_Lisp_T(struct LMud_Lisp* self)
{
    return self->constants.t;
}

LMud_Any LMud_Lisp_Nil(struct LMud_Lisp* self)
{
    return self->constants.nil;
}

LMud_Any LMud_Lisp_Boolean(struct LMud_Lisp* self, bool value)
{
    if (value)
        return LMud_Lisp_T(self);
    else
        return LMud_Lisp_Nil(self);
}

bool LMud_Lisp_Truthy(struct LMud_Lisp* self, LMud_Any value)
{
    return !LMud_Any_Eq(value, LMud_Lisp_Nil(self));
}

LMud_Any LMud_Lisp_CustomDispatcherFunction(struct LMud_Lisp* self)
{
    return self->constants.custom_dispatcher_function;
}

void LMud_Lisp_SetCustomDispatcherFunction(struct LMud_Lisp* self, LMud_Any value)
{
    self->constants.custom_dispatcher_function = value;
}

void LMud_Lisp_SetNewConnectionFunction(struct LMud_Lisp* self, LMud_Any value)
{
    self->constants.new_connection_function = value;
}


LMud_Any LMud_Lisp_MakeArray(struct LMud_Lisp* self, LMud_Size size, LMud_Any fill)
{
    return LMud_Any_FromPointer(LMud_Objects_MakeArray(&self->objects, size, fill));
}

LMud_Any LMud_Lisp_MakeArray_FromData(struct LMud_Lisp* self, LMud_Size size, LMud_Any* data)
{
    return LMud_Any_FromPointer(LMud_Objects_MakeArray_FromData(&self->objects, size, data));
}

LMud_Any LMud_Lisp_Builtin(struct LMud_Lisp* self, const char* name, LMud_BuiltinFunction function)
{
    return LMud_Any_FromPointer(LMud_Objects_Builtin(&self->objects, name, function));
}

LMud_Any LMud_Lisp_MakeBytes(struct LMud_Lisp* self, LMud_Size size)
{
    return LMud_Any_FromPointer(LMud_Objects_MakeBytes(&self->objects, size));
}

LMud_Any LMud_Lisp_MakeBytes_FromData(struct LMud_Lisp* self, LMud_Size size, const char* data)
{
    return LMud_Any_FromPointer(LMud_Objects_MakeBytes_FromData(&self->objects, size, data));
}

LMud_Any LMud_Lisp_Closure(struct LMud_Lisp* self, struct LMud_Function* function, struct LMud_Frame* lexical)
{
    return LMud_Any_FromPointer(LMud_Objects_Closure(&self->objects, function, lexical));
}

LMud_Any LMud_Lisp_Cons(struct LMud_Lisp* self, LMud_Any car, LMud_Any cdr)
{
    return LMud_Any_FromPointer(LMud_Objects_Cons(&self->objects, car, cdr));
}

LMud_Any LMud_Lisp_Custom(struct LMud_Lisp* self, LMud_Any meta, LMud_Any* slots, LMud_Size size)
{
    return LMud_Any_FromPointer(LMud_Objects_Custom(&self->objects, meta, slots, size));
}

LMud_Any LMud_Lisp_Function(struct LMud_Lisp* self, struct LMud_ArgInfo info, LMud_Any bytecodes, LMud_Any constants, LMud_Any source_code)
{
    return LMud_Any_FromPointer(LMud_Objects_Function(&self->objects, info, bytecodes, constants, source_code));
}

LMud_Any LMud_Lisp_Package(struct LMud_Lisp* self, LMud_Any name)
{
    return LMud_Any_FromPointer(LMud_Objects_Package(&self->objects, name));
}

LMud_Any LMud_Lisp_PackageByName(struct LMud_Lisp* self, const char* name)
{
    return LMud_Any_FromPointer(LMud_Objects_Package(&self->objects, LMud_Lisp_String(self, name)));
}

LMud_Any LMud_Lisp_PackageByNameUpcase(struct LMud_Lisp* self, const char* name)
{
    struct LMud_StringBuilder  builder;
    LMud_Any                   result;

    LMud_StringBuilder_Create(&builder);
    LMud_StringBuilder_AppendCStr_Uppercased(&builder, name);
    result = LMud_Lisp_PackageByName(self, LMud_StringBuilder_GetStatic(&builder));
    LMud_StringBuilder_Destroy(&builder);

    return result;
}

LMud_Any LMud_Lisp_Port(struct LMud_Lisp* self, struct LMud_Connection* connection)
{
    return LMud_Any_FromPointer(LMud_Objects_Port(&self->objects, connection));
}

LMud_Any LMud_Lisp_Process(struct LMud_Lisp* self, struct LMud_Fiber* fiber, struct LMud_Process** slot)
{
    return LMud_Any_FromPointer(LMud_Objects_Process(&self->objects, fiber, slot));
}

LMud_Any LMud_Lisp_Ratio(struct LMud_Lisp* self, LMud_Any numerator, LMud_Any denominator)
{
    return LMud_Any_FromPointer(LMud_Objects_Ratio(&self->objects, numerator, denominator));
}

LMud_Any LMud_Lisp_StackFrame(struct LMud_Lisp* self, struct LMud_Frame* frame, struct LMud_StackFrame** slot)
{
    return LMud_Any_FromPointer(LMud_Objects_StackFrame(&self->objects, frame, slot));
}

LMud_Any LMud_Lisp_String(struct LMud_Lisp* self, const char* text)
{
    return LMud_Any_FromPointer(LMud_Objects_String(&self->objects, text));
}

LMud_Any LMud_Lisp_InternInPackage(struct LMud_Lisp* self, LMud_Any package, const char* name)
{
    struct LMud_Symbol*  symbol;

    assert(LMud_Lisp_IsPackage(self, package));

    symbol = LMud_Objects_Intern(&self->objects, LMud_Any_AsPointer(package), name);

    /*
     * If we are interning a symbol in the KEYWORD package, we make it a self-referencing constant.
     */
    if (LMud_Any_Eq(package, self->constants.keyword_package))
    {
        LMud_Symbol_MakeConstant(symbol, true);
    }

    return LMud_Any_FromPointer(symbol);
}

LMud_Any LMud_Lisp_Intern(struct LMud_Lisp* self, const char* name)
{
    return LMud_Lisp_InternInPackage(self, self->constants.default_package, name);
}

LMud_Any LMud_Lisp_InternUpcaseInPackage(struct LMud_Lisp* self, LMud_Any package, const char* name)
{
    struct LMud_StringBuilder  builder;
    LMud_Any                   result;

    LMud_StringBuilder_Create(&builder);
    LMud_StringBuilder_AppendCStr_Uppercased(&builder, name);
    result = LMud_Lisp_InternInPackage(self, package, LMud_StringBuilder_GetStatic(&builder));
    LMud_StringBuilder_Destroy(&builder);

    return result;
}

LMud_Any LMud_Lisp_InternUpcase(struct LMud_Lisp* self, const char* name)
{
    return LMud_Lisp_InternUpcaseInPackage(self, self->constants.default_package, name);
}

LMud_Any LMud_Lisp_InternKeyword(struct LMud_Lisp* self, const char* name)
{
    return LMud_Lisp_InternInPackage(self, self->constants.keyword_package, name);
}

LMud_Any LMud_Lisp_InternKeywordUpcase(struct LMud_Lisp* self, const char* name)
{
    return LMud_Lisp_InternUpcaseInPackage(self, self->constants.keyword_package, name);
}

LMud_Any LMud_Lisp_EasyIntern(struct LMud_Lisp* self, const char* package_name, const char* name)
{
    return LMud_Lisp_InternInPackage(self, LMud_Lisp_PackageByName(self, package_name), name);
}

LMud_Any LMud_Lisp_EasyUpcaseIntern(struct LMud_Lisp* self, const char* package_name, const char* name)
{
    return LMud_Lisp_InternUpcaseInPackage(self, LMud_Lisp_PackageByNameUpcase(self, package_name), name);
}

LMud_Any LMud_Lisp_ReinternAsKeyword(struct LMud_Lisp* self, LMud_Any symbol)
{
    assert(LMud_Lisp_IsSymbol(self, symbol));
    return LMud_Lisp_InternKeyword(self, LMud_Symbol_NameChars(LMud_Any_AsPointer(symbol)));
}

LMud_Any LMud_Lisp_InternInPackageLL(struct LMud_Lisp* self, LMud_Any package, LMud_Any name)
{
    struct LMud_Symbol*  symbol;

    assert(LMud_Lisp_IsPackage(self, package));
    assert(LMud_Lisp_IsString(self, name));

    symbol = LMud_Objects_Intern(&self->objects, LMud_Any_AsPointer(package), LMud_String_Chars(LMud_Any_AsPointer(name)));

    /*
     * If we are interning a symbol in the KEYWORD package, we make it a self-referencing constant.
     */
    if (LMud_Any_Eq(package, self->constants.keyword_package))
    {
        LMud_Symbol_MakeConstant(symbol, true);
    }

    return LMud_Any_FromPointer(symbol);
}

LMud_Any LMud_Lisp_Gensym(struct LMud_Lisp* self)
{
    return LMud_Any_FromPointer(LMud_Objects_Gensym(&self->objects));
}


LMud_Any LMud_Lisp_Car(struct LMud_Lisp* self, LMud_Any value)
{
    if (!LMud_Lisp_IsCons(self, value))
        return value;
    return ((struct LMud_Cons*) LMud_Any_AsPointer(value))->car;
}

bool LMud_Lisp_TakeNext(struct LMud_Lisp* self, LMud_Any* value, LMud_Any* result)
{
    if (!LMud_Lisp_IsCons(self, *value)) {
        *result = LMud_Lisp_Nil(self);
        return false;
    }

    *result = LMud_Lisp_Car(self, *value);
    *value  = LMud_Lisp_Cdr(self, *value);

    return true;
}

LMud_Any LMud_Lisp_Cdr(struct LMud_Lisp* self, LMud_Any value)
{
    if (!LMud_Lisp_IsCons(self, value))
        return value;
    return ((struct LMud_Cons*) LMud_Any_AsPointer(value))->cdr;
}

LMud_Any LMud_Lisp_Caar(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Lisp_Car(self, LMud_Lisp_Car(self, value));
}

LMud_Any LMud_Lisp_Cadr(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Lisp_Car(self, LMud_Lisp_Cdr(self, value));
}

LMud_Any LMud_Lisp_Cdar(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Lisp_Cdr(self, LMud_Lisp_Car(self, value));
}

LMud_Any LMud_Lisp_Cddr(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Lisp_Cdr(self, LMud_Lisp_Cdr(self, value));
}

bool LMud_Lisp_Rplaca(struct LMud_Lisp* self, LMud_Any cons, LMud_Any value)
{
    if (!LMud_Lisp_IsCons(self, cons))
        return false;
    ((struct LMud_Cons*) LMud_Any_AsPointer(cons))->car = value;
    return true;
}

bool LMud_Lisp_Rplacd(struct LMud_Lisp* self, LMud_Any cons, LMud_Any value)
{
    if (!LMud_Lisp_IsCons(self, cons))
        return false;
    ((struct LMud_Cons*) LMud_Any_AsPointer(cons))->cdr = value;
    return true;
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


bool LMud_Lisp_Aref(struct LMud_Lisp* self, LMud_Any object, LMud_Any index, LMud_Any* result)
{
    LMud_Size  index_value;
    LMud_Rune  rune;

    if (!LMud_Any_IsInteger(index))
        return false;
    
    *result     = LMud_Lisp_Nil(self);
    index_value = LMud_Any_AsInteger(index);

    if (LMud_Lisp_IsArray(self, object)) {
        *result = LMud_Array_Aref(LMud_Any_AsPointer(object), index_value, LMud_Lisp_Nil(self));
        return true;
    } else if (LMud_Lisp_IsBytes(self, object)) {
        *result = LMud_Bytes_Aref(LMud_Any_AsPointer(object), index_value);
        return true;
    } else if (LMud_Lisp_IsString(self, object)) {
        if (!LMud_String_RuneAt(LMud_Any_AsPointer(object), index_value, &rune))
            return false;
        *result = LMud_Any_FromCharacter(rune);
        return true;
    } else {
        return false;
    }
}

bool LMud_Lisp_Aset(struct LMud_Lisp* self, LMud_Any object, LMud_Any index, LMud_Any value)
{
    LMud_Size  index_value;

    if (!LMud_Any_IsInteger(index))
        return false;
    
    index_value = LMud_Any_AsInteger(index);

    if (LMud_Lisp_IsArray(self, object)) {
        LMud_Array_Aset(LMud_Any_AsPointer(object), index_value, value);
        return true;
    } else if (LMud_Lisp_IsBytes(self, object)) {
        LMud_Bytes_Aset(LMud_Any_AsPointer(object), index_value, value);
        return true;
    } else {
        return false;
    }
}


LMud_Any LMud_Lisp_Quote(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Lisp_Cons(self, self->constants.quote, LMud_Lisp_Cons(self, value, LMud_Lisp_Nil(self)));
}

LMud_Any LMud_Lisp_QuoteFunction(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Lisp_Cons(self, self->constants.function, LMud_Lisp_Cons(self, value, LMud_Lisp_Nil(self)));
}


bool LMud_Lisp_Compile(struct LMud_Lisp* self, LMud_Any expression, LMud_Any* result)
{
    struct LMud_CompilerSession  session;
    struct LMud_Compiler         compiler;
    LMud_Any                     function;

    LMud_CompilerSession_Create(&session, self);
    LMud_Compiler_Create(&compiler, &session);

    LMud_Compiler_BeginBlock(&compiler, LMud_Lisp_Nil(self));
    LMud_Compiler_Compile(&compiler, expression);
    LMud_Compiler_EndBlock(&compiler);

    function = LMud_Compiler_Build(&compiler);

    LMud_Compiler_Destroy(&compiler);
    LMud_CompilerSession_Destroy(&session);

    *result = function;

    return true;
}


void LMud_Lisp_InstallBuiltin(struct LMud_Lisp* self, const char* name, LMud_BuiltinFunction function)
{
    struct LMud_Symbol*  symbol;
    LMud_Any             builtin;

    symbol  = LMud_Any_AsPointer(LMud_Lisp_Intern(self, name));
    builtin = LMud_Lisp_Builtin(self, name, function);

    LMud_Symbol_SetFunction(symbol, builtin, true);
}

void LMud_Lisp_InstallPackagedBuiltin(struct LMud_Lisp* self, const char* package_name, const char* name, LMud_BuiltinFunction function)
{
    struct LMud_Symbol*  symbol;
    LMud_Any             builtin;

    symbol  = LMud_Any_AsPointer(LMud_Lisp_EasyIntern(self, package_name, name));
    builtin = LMud_Lisp_Builtin(self, name, function);

    LMud_Symbol_SetFunction(symbol, builtin, true);
}


void LMud_Lisp_GarbageCollect(struct LMud_Lisp* self, struct LMud_GCStats* stats)
{
    struct LMud_GC  gc;

    LMud_GC_Create(&gc, self);
    LMud_GC_Run(&gc);
    LMud_GC_FetchStats(&gc, stats);
    LMud_GC_Destroy(&gc);
}


bool LMud_Lisp_NeedsControlBackImmediately(struct LMud_Lisp* self)
{
    return LMud_Scheduler_NeedsControlBackImmediately(&self->scheduler);
}

void LMud_Lisp_PeriodicInterrupt(struct LMud_Lisp* self)
{
    if (LMud_Objects_GetGcAllocationCounter(&self->objects) >= (16 * 1024 * 1024))
    {
        LMud_Lisp_GarbageCollect(self, NULL);
    }
}

void LMud_Lisp_Tick(struct LMud_Lisp* self)
{
    LMud_Scheduler_Tick(&self->scheduler);
    LMud_Lisp_PeriodicInterrupt(self);
}


bool LMud_Lisp_Kickstart(struct LMud_Lisp* self, struct LMud_Profile* profile, LMud_Any function)
{
    return LMud_Scheduler_Kickstart(&self->scheduler, profile, function) != NULL;
}

bool LMud_Lisp_KickstartNewConnectionTask(struct LMud_Lisp* self, struct LMud_Profile* profile, LMud_Any function, struct LMud_Connection* connection)
{
    LMud_Any  connection_any;

    connection_any = LMud_Lisp_Port(self, connection);

    return LMud_Scheduler_KickstartWithArgs(&self->scheduler, profile, function, &connection_any, 1);
}

LMud_Any LMud_Lisp_KickstartProcess(struct LMud_Lisp* self, struct LMud_Profile* profile, LMud_Any function, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_Fiber*  fiber;
    
    fiber = LMud_Scheduler_KickstartWithArgs(&self->scheduler, profile, function, arguments, argument_count);

    return (fiber == NULL) ? LMud_Lisp_Nil(self) : LMud_Any_FromPointer(LMud_Fiber_GetProcess(fiber, self));
}


bool LMud_Lisp_LoadFile(struct LMud_Lisp* self, struct LMud_Profile* profile, const char* filename, LMud_Any* result)
{
    struct LMud_InputStream  stream;
    FILE*                    file;
    LMud_Any                 program;

    file = fopen(filename, "r");
    
    if (file == NULL)
        LMud_Logf(self->mud, LMud_LogLevel_ERROR, "Failed to open file: \"%s\"!\n", filename);
    else {
        LMud_Logf(self->mud, LMud_LogLevel_INFO, "Loading file: \"%s\"...\n", filename);

        LMud_InputStream_CreateFromFile(&stream, file);

        while (LMud_Lisp_Read(self, &stream, &program))
        {
            if (!LMud_Lisp_Compile(self, program, &program)) {
                LMud_Logf(self->mud, LMud_LogLevel_WARNING, " --> Failed to compile an expression!\n");
                break;
            } else {
                LMud_Scheduler_BlockAndRunThunk(&self->scheduler, profile, program, result);
            }
        }

        LMud_InputStream_Destroy(&stream);
        fclose(file);
    }

    return true; // TODO: Return false on error
}
