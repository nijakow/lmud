
#include <lmud/lisp/builtins.h>
#include <lmud/lisp/compiler/compiler.h>
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


LMud_Any LMud_Lisp_Numerator(struct LMud_Lisp* self, LMud_Any value)
{
    if (LMud_Lisp_IsRatio(self, value))
        return LMud_Ratio_Numerator(LMud_Any_AsPointer(value));
    else
        return value;
}

LMud_Any LMud_Lisp_Denominator(struct LMud_Lisp* self, LMud_Any value)
{
    if (LMud_Lisp_IsRatio(self, value))
        return LMud_Ratio_Denominator(LMud_Any_AsPointer(value));
    else
        return LMud_Any_FromInteger(1);
}

bool LMud_Lisp_IntegerEqual(struct LMud_Lisp* self, LMud_Any a, LMud_Any b)
{
    (void) self;
    if (LMud_Any_IsInteger(a) && LMud_Any_IsInteger(b))
        return LMud_Any_AsInteger(a) == LMud_Any_AsInteger(b);
    else
        return false;
}

bool LMud_Lisp_NumericEqual(struct LMud_Lisp* self, LMud_Any a, LMud_Any b)
{
    return LMud_Lisp_IntegerEqual(self, LMud_Lisp_Numerator(self, a), LMud_Lisp_Numerator(self, b))
        && LMud_Lisp_IntegerEqual(self, LMud_Lisp_Denominator(self, a), LMud_Lisp_Denominator(self, b));
}

bool LMud_Lisp_Gcd(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
{
    LMud_Integer  a_value;
    LMud_Integer  b_value;
    LMud_Integer  temp;

    (void) self;

    if (!LMud_Any_IsInteger(a) || !LMud_Any_IsInteger(b))
        return false;

    a_value = LMud_Any_AsInteger(a);
    b_value = LMud_Any_AsInteger(b);

    while (b_value != 0)
    {
        temp    = b_value;
        b_value = a_value % b_value;
        a_value = temp;
    }

    *result = LMud_Any_FromInteger(a_value);

    return true;
}


bool LMud_Lisp_IntegerAdd2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
{
    LMud_Integer  a_value;
    LMud_Integer  b_value;

    (void) self;

    if (!LMud_Any_IsInteger(a) || !LMud_Any_IsInteger(b))
        return false;

    a_value = LMud_Any_AsInteger(a);
    b_value = LMud_Any_AsInteger(b);

    *result = LMud_Any_FromInteger(a_value + b_value);

    return true;
}

bool LMud_Lisp_IntegerSub2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
{
    LMud_Integer  a_value;
    LMud_Integer  b_value;

    (void) self;

    if (!LMud_Any_IsInteger(a) || !LMud_Any_IsInteger(b))
        return false;

    a_value = LMud_Any_AsInteger(a);
    b_value = LMud_Any_AsInteger(b);

    *result = LMud_Any_FromInteger(a_value - b_value);

    return true;
}

bool LMud_Lisp_IntegerMul2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
{
    LMud_Integer  a_value;
    LMud_Integer  b_value;

    (void) self;

    if (!LMud_Any_IsInteger(a) || !LMud_Any_IsInteger(b))
        return false;

    a_value = LMud_Any_AsInteger(a);
    b_value = LMud_Any_AsInteger(b);

    *result = LMud_Any_FromInteger(a_value * b_value);

    return true;
}

bool LMud_Lisp_IntegerDiv2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
{
    LMud_Integer  a_value;
    LMud_Integer  b_value;

    (void) self;

    if (!LMud_Any_IsInteger(a) || !LMud_Any_IsInteger(b))
        return false;

    a_value = LMud_Any_AsInteger(a);
    b_value = LMud_Any_AsInteger(b);

    *result = LMud_Any_FromInteger(a_value / b_value);

    return true;
}

bool LMud_Lisp_IntegerMod2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
{
    LMud_Integer  a_value;
    LMud_Integer  b_value;

    (void) self;

    if (!LMud_Any_IsInteger(a) || !LMud_Any_IsInteger(b))
        return false;

    a_value = LMud_Any_AsInteger(a);
    b_value = LMud_Any_AsInteger(b);

    *result = LMud_Any_FromInteger(a_value % b_value);

    return true;
}

bool LMud_Lisp_IntegerNegate(struct LMud_Lisp* self, LMud_Any a, LMud_Any* result)
{
    LMud_Integer  a_value;

    (void) self;

    if (!LMud_Any_IsInteger(a))
        return false;

    a_value = LMud_Any_AsInteger(a);

    *result = LMud_Any_FromInteger(-a_value);

    return true;
}

bool LMud_Lisp_RatioOrInteger(struct LMud_Lisp* self, LMud_Any numerator, LMud_Any denominator, LMud_Any* result)
{
    LMud_Any  gcd;
    LMud_Any  new_numerator;
    LMud_Any  new_denominator;

    if (!LMud_Lisp_Gcd(self, numerator, denominator, &gcd))
        return false;

    /*
     * TODO: Catch the results of these functions and return false if they fail.
     */
    LMud_Lisp_IntegerDiv2(self, numerator, gcd, &new_numerator);
    LMud_Lisp_IntegerDiv2(self, denominator, gcd, &new_denominator);

    // TODO
    if (LMud_Any_IsInteger(new_denominator) && LMud_Any_AsInteger(new_denominator) < 0) {
        LMud_Lisp_IntegerNegate(self, new_numerator, &new_numerator);
        LMud_Lisp_IntegerNegate(self, new_denominator, &new_denominator);
    }

    if (LMud_Lisp_NumericEqual(self, new_denominator, LMud_Any_FromInteger(1))) {
        *result = new_numerator;
    } else {
        *result = LMud_Lisp_Ratio(self, new_numerator, new_denominator);
    }

    return true;
}

bool LMud_Lisp_AdjustRatios(struct LMud_Lisp* self,
                            LMud_Any n1,
                            LMud_Any d1,
                            LMud_Any n2,
                            LMud_Any d2,
                            LMud_Any* tn1,
                            LMud_Any* tn2,
                            LMud_Any* td)
{
    return LMud_Lisp_IntegerMul2(self, d1, d2, td)
        && LMud_Lisp_IntegerMul2(self, n1, d2, tn1)
        && LMud_Lisp_IntegerMul2(self, n2, d1, tn2);
}

typedef bool (*LMud_RatioArithmeticFunction)(struct LMud_Lisp* self, LMud_Any n1, LMud_Any d1, LMud_Any n2, LMud_Any d2, LMud_Any* result);

bool LMud_Lisp_NumberDispatch(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_RatioArithmeticFunction func, LMud_Any* result)
{
    if (LMud_Any_IsInteger(a) && LMud_Any_IsInteger(b)) {
        return func(
                    self,
                    a,
                    LMud_Any_FromInteger(1),
                    b,
                    LMud_Any_FromInteger(1),
                    result
        );
    } else if (LMud_Lisp_IsRatio(self, a) && LMud_Any_IsInteger(b)) {
        return func(
                    self,
                    LMud_Ratio_Numerator(LMud_Any_AsPointer(a)),
                    LMud_Ratio_Denominator(LMud_Any_AsPointer(a)),
                    b,
                    LMud_Any_FromInteger(1),
                    result
        );
    } else if (LMud_Any_IsInteger(a) && LMud_Lisp_IsRatio(self, b)) {
        return func(
                    self,
                    a,
                    LMud_Any_FromInteger(1),
                    LMud_Ratio_Numerator(LMud_Any_AsPointer(b)),
                    LMud_Ratio_Denominator(LMud_Any_AsPointer(b)),
                    result
        );
    } else if (LMud_Lisp_IsRatio(self, a) && LMud_Lisp_IsRatio(self, b)) {
        return func(
                    self,
                    LMud_Ratio_Numerator(LMud_Any_AsPointer(a)),
                    LMud_Ratio_Denominator(LMud_Any_AsPointer(a)),
                    LMud_Ratio_Numerator(LMud_Any_AsPointer(b)),
                    LMud_Ratio_Denominator(LMud_Any_AsPointer(b)),
                    result
        );
    } else {
        *result = LMud_Lisp_Nil(self);
        return false;
    }
}

bool LMud_Lisp_Add2_Ratios(struct LMud_Lisp* self, LMud_Any n1, LMud_Any d1, LMud_Any n2, LMud_Any d2, LMud_Any* result)
{
    LMud_Any  numerator;
    LMud_Any  denominator;
    LMud_Any  adjusted_n1;
    LMud_Any  adjusted_n2;

    return LMud_Lisp_AdjustRatios(self, n1, d1, n2, d2, &adjusted_n1, &adjusted_n2, &denominator)
        && LMud_Lisp_IntegerAdd2(self, adjusted_n1, adjusted_n2, &numerator)
        && LMud_Lisp_RatioOrInteger(self, numerator, denominator, result);
}

bool LMud_Lisp_Add2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
{
    if (LMud_Any_IsInteger(a) && LMud_Any_IsInteger(b)) {
        *result = LMud_Any_FromInteger(LMud_Any_AsInteger(a) + LMud_Any_AsInteger(b));
        return true;
    } else {
        return LMud_Lisp_NumberDispatch(self, a, b, LMud_Lisp_Add2_Ratios, result);
    }
}

bool LMud_Lisp_Sub2_Ratios(struct LMud_Lisp* self, LMud_Any n1, LMud_Any d1, LMud_Any n2, LMud_Any d2, LMud_Any* result)
{
    LMud_Any  numerator;
    LMud_Any  denominator;
    LMud_Any  adjusted_n1;
    LMud_Any  adjusted_n2;

    return LMud_Lisp_AdjustRatios(self, n1, d1, n2, d2, &adjusted_n1, &adjusted_n2, &denominator)
        && LMud_Lisp_IntegerSub2(self, adjusted_n1, adjusted_n2, &numerator)
        && LMud_Lisp_RatioOrInteger(self, numerator, denominator, result);
}

bool LMud_Lisp_Sub2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
{
    return LMud_Lisp_NumberDispatch(self, a, b, LMud_Lisp_Sub2_Ratios, result);
}

bool LMud_Lisp_Mul2_Ratios(struct LMud_Lisp* self, LMud_Any n1, LMud_Any d1, LMud_Any n2, LMud_Any d2, LMud_Any* result)
{
    LMud_Any  numerator;
    LMud_Any  denominator;

    return LMud_Lisp_IntegerMul2(self, n1, n2, &numerator)
        && LMud_Lisp_IntegerMul2(self, d1, d2, &denominator)
        && LMud_Lisp_RatioOrInteger(self, numerator, denominator, result);
}

bool LMud_Lisp_Mul2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
{
    return LMud_Lisp_NumberDispatch(self, a, b, LMud_Lisp_Mul2_Ratios, result);
}

bool LMud_Lisp_Div2_Ratios(struct LMud_Lisp* self, LMud_Any n1, LMud_Any d1, LMud_Any n2, LMud_Any d2, LMud_Any* result)
{
    return LMud_Lisp_Mul2_Ratios(self, n1, d1, d2, n2, result);
}

bool LMud_Lisp_Div2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
{
    return LMud_Lisp_NumberDispatch(self, a, b, LMud_Lisp_Div2_Ratios, result);
}

bool LMud_Lisp_Mod2_Ratios(struct LMud_Lisp* self, LMud_Any n1, LMud_Any d1, LMud_Any n2, LMud_Any d2, LMud_Any* result)
{
    LMud_Any  numerator;
    LMud_Any  denominator;
    LMud_Any  adjusted_n1;
    LMud_Any  adjusted_n2;

    return LMud_Lisp_AdjustRatios(self, n1, d1, n2, d2, &adjusted_n1, &adjusted_n2, &denominator)
        && LMud_Lisp_IntegerMod2(self, adjusted_n1, adjusted_n2, &numerator)
        && LMud_Lisp_RatioOrInteger(self, numerator, denominator, result);
}

bool LMud_Lisp_Mod2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
{
    return LMud_Lisp_NumberDispatch(self, a, b, LMud_Lisp_Mod2_Ratios, result);
}
