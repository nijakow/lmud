
#include <lmud/lisp/builtins.h>
#include <lmud/lisp/compiler/compiler.h>
#include <lmud/lisp/io.h>
#include <lmud/util/stringbuilder.h>

#include "lisp.h"


bool LMud_Constants_Create(struct LMud_Constants* self, struct LMud_Lisp* lisp)
{
    /*
     * We have a bit of a chicken-and-egg problem here. We need to create the
     * NIL and T symbols, but we need to have the lisp object to initialize
     * them. So we have to do this manually.
     */
    self->nil      = LMud_Lisp_Intern(lisp, "NIL");
    LMud_Symbol_SetValue(LMud_Any_AsPointer(self->nil), self->nil);
    LMud_Symbol_SetFunction(LMud_Any_AsPointer(self->nil), self->nil);
    LMud_Symbol_SetMacro(LMud_Any_AsPointer(self->nil), self->nil);
    self->t        = LMud_Lisp_Intern(lisp, "T");
    LMud_Symbol_MakeConstant(LMud_Any_AsPointer(self->nil));
    LMud_Symbol_MakeConstant(LMud_Any_AsPointer(self->t));

    self->quote    = LMud_Lisp_Intern(lisp, "QUOTE");
    self->function = LMud_Lisp_Intern(lisp, "FUNCTION");

    LMud_Lisp_InstallBuiltins(lisp);

    return true;
}

void LMud_Constants_Destroy(struct LMud_Constants* self)
{
    (void) self;
}



bool LMud_Lisp_Create(struct LMud_Lisp* self)
{
    return LMud_Objects_Create(&self->objects, self)
        && LMud_Constants_Create(&self->constants, self)
        && LMud_Scheduler_Create(&self->scheduler, self);
}

void LMud_Lisp_Destroy(struct LMud_Lisp* self)
{
    LMud_Scheduler_Destroy(&self->scheduler);
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

bool LMud_Lisp_IsFunctionPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsFunction(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsRatioPointer(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsRatio(LMud_Lisp_Types(self), object);
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

bool LMud_Lisp_IsFunction(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsFunctionPointer(self, LMud_Any_AsPointer(value));
}

bool LMud_Lisp_IsRatio(struct LMud_Lisp* self, LMud_Any value)
{
    return LMud_Any_IsPointer(value) && LMud_Lisp_IsRatioPointer(self, LMud_Any_AsPointer(value));
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

LMud_Any LMud_Lisp_Function(struct LMud_Lisp* self, struct LMud_ArgInfo info, LMud_Any bytecodes, LMud_Any constants)
{
    return LMud_Any_FromPointer(LMud_Objects_Function(&self->objects, info, bytecodes, constants));
}

LMud_Any LMud_Lisp_Ratio(struct LMud_Lisp* self, LMud_Any numerator, LMud_Any denominator)
{
    return LMud_Any_FromPointer(LMud_Objects_Ratio(&self->objects, numerator, denominator));
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

    if (!LMud_Any_IsInteger(index))
        return false;
    
    index_value = LMud_Any_AsInteger(index);

    if (LMud_Lisp_IsArray(self, object)) {
        *result = LMud_Array_Aref(LMud_Any_AsPointer(object), index_value, LMud_Lisp_Nil(self));
        return true;
    } else if (LMud_Lisp_IsBytes(self, object)) {
        *result = LMud_Bytes_Aref(LMud_Any_AsPointer(object), index_value);
        return true;
    } else {
        *result = LMud_Lisp_Nil(self);
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

    LMud_Compiler_Compile(&compiler, expression);

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

    symbol  = LMud_Objects_Intern(&self->objects, name);
    builtin = LMud_Lisp_Builtin(self, name, function);

    LMud_Symbol_SetFunction(symbol, builtin);
}


void LMud_Lisp_LoadFile(struct LMud_Lisp* self, const char* filename)
{
    struct LMud_InputStream  stream;
    FILE*                    file;
    LMud_Any                 program;

    file = fopen(filename, "r");

    if (file == NULL)
        fprintf(stderr, "; Failed to open file: \"%s\"\n", filename);
    else {
        printf("; Loading file: \"%s\"...\n", filename);

        LMud_InputStream_CreateFromFile(&stream, file);

        while (LMud_Lisp_Read(self, &stream, &program))
        {
            printf(";   --> Read an expression\n");
            // TODO
        }

        LMud_InputStream_Destroy(&stream);        
        fclose(file);
    }
}
