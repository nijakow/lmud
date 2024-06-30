
#include <lmud/lisp/lisp.h>

#include "math.h"


bool LMud_Lisp_IntegerLess(struct LMud_Lisp* self, LMud_Any a, LMud_Any b)
{
    LMud_Integer  a_value;
    LMud_Integer  b_value;

    (void) self;

    if (!LMud_Any_IsInteger(a) || !LMud_Any_IsInteger(b))
        return false;

    a_value = LMud_Any_AsInteger(a);
    b_value = LMud_Any_AsInteger(b);

    return a_value < b_value;
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


typedef bool (*LMud_RatioArithmeticFunction)(struct LMud_Lisp* self, LMud_Any n1, LMud_Any d1, LMud_Any n2, LMud_Any d2, void* result);

bool LMud_Lisp_NumberDispatch(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_RatioArithmeticFunction func, void* result)
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
        return false;
    }
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

bool LMud_Lisp_NumericLess_Ratios(struct LMud_Lisp* self, LMud_Any n1, LMud_Any d1, LMud_Any n2, LMud_Any d2, bool* result)
{
    LMud_Any  adjusted_n1;
    LMud_Any  adjusted_n2;
    LMud_Any  adjusted_d;

    if (!LMud_Lisp_AdjustRatios(self, n1, d1, n2, d2, &adjusted_n1, &adjusted_n2, &adjusted_d))
        return false;

    *result = LMud_Lisp_IntegerLess(self, adjusted_n1, adjusted_n2);

    return true;
}

bool LMud_Lisp_NumericLess(struct LMud_Lisp* self, LMud_Any a, LMud_Any b)
{
    bool  result;

    LMud_Lisp_NumberDispatch(self, a, b, (LMud_RatioArithmeticFunction) LMud_Lisp_NumericLess_Ratios, &result);

    return result;
}

bool LMud_Lisp_NumericLessEqual(struct LMud_Lisp* self, LMud_Any a, LMud_Any b)
{
    return LMud_Lisp_NumericEqual(self, a, b) || LMud_Lisp_NumericLess(self, a, b);
}

bool LMud_Lisp_NumericGreater(struct LMud_Lisp* self, LMud_Any a, LMud_Any b)
{
    return !LMud_Lisp_NumericLessEqual(self, a, b);
}

bool LMud_Lisp_NumericGreaterEqual(struct LMud_Lisp* self, LMud_Any a, LMud_Any b)
{
    return !LMud_Lisp_NumericLess(self, a, b);
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
        return LMud_Lisp_NumberDispatch(self, a, b, (LMud_RatioArithmeticFunction) LMud_Lisp_Add2_Ratios, result);
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
    return LMud_Lisp_NumberDispatch(self, a, b, (LMud_RatioArithmeticFunction) LMud_Lisp_Sub2_Ratios, result);
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
    return LMud_Lisp_NumberDispatch(self, a, b, (LMud_RatioArithmeticFunction) LMud_Lisp_Mul2_Ratios, result);
}

bool LMud_Lisp_Div2_Ratios(struct LMud_Lisp* self, LMud_Any n1, LMud_Any d1, LMud_Any n2, LMud_Any d2, LMud_Any* result)
{
    return LMud_Lisp_Mul2_Ratios(self, n1, d1, d2, n2, result);
}

bool LMud_Lisp_Div2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
{
    return LMud_Lisp_NumberDispatch(self, a, b, (LMud_RatioArithmeticFunction) LMud_Lisp_Div2_Ratios, result);
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
    return LMud_Lisp_NumberDispatch(self, a, b, (LMud_RatioArithmeticFunction) LMud_Lisp_Mod2_Ratios, result);
}

bool LMud_Lisp_Truncate(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
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

bool LMud_Lisp_LogAnd2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
{
    LMud_Integer  a_value;
    LMud_Integer  b_value;

    (void) self;

    if (!LMud_Any_IsInteger(a) || !LMud_Any_IsInteger(b))
        return false;

    a_value = LMud_Any_AsInteger(a);
    b_value = LMud_Any_AsInteger(b);

    *result = LMud_Any_FromInteger(a_value & b_value);

    return true;
}

bool LMud_Lisp_LogIor2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
{
    LMud_Integer  a_value;
    LMud_Integer  b_value;

    (void) self;

    if (!LMud_Any_IsInteger(a) || !LMud_Any_IsInteger(b))
        return false;

    a_value = LMud_Any_AsInteger(a);
    b_value = LMud_Any_AsInteger(b);

    *result = LMud_Any_FromInteger(a_value | b_value);

    return true;
}

bool LMud_Lisp_LogXor2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
{
    LMud_Integer  a_value;
    LMud_Integer  b_value;

    (void) self;

    if (!LMud_Any_IsInteger(a) || !LMud_Any_IsInteger(b))
        return false;

    a_value = LMud_Any_AsInteger(a);
    b_value = LMud_Any_AsInteger(b);

    *result = LMud_Any_FromInteger(a_value ^ b_value);

    return true;
}

bool LMud_Lisp_LogNot(struct LMud_Lisp* self, LMud_Any a, LMud_Any* result)
{
    LMud_Integer  a_value;

    (void) self;

    if (!LMud_Any_IsInteger(a))
        return false;

    a_value = LMud_Any_AsInteger(a);

    *result = LMud_Any_FromInteger(~a_value);

    return true;
}

bool LMud_Lisp_Ash(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result)
{
    LMud_Integer  a_value;
    LMud_Integer  b_value;

    (void) self;

    if (!LMud_Any_IsInteger(a) || !LMud_Any_IsInteger(b))
        return false;

    a_value = LMud_Any_AsInteger(a);
    b_value = LMud_Any_AsInteger(b);

    if (b_value < 0)
        *result = LMud_Any_FromInteger(a_value >> -b_value);
    else
        *result = LMud_Any_FromInteger(a_value << b_value);

    return true;
}
