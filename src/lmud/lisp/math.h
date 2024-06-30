
#pragma once

#include <lmud/lisp/base.h>

LMud_Any LMud_Lisp_Numerator(struct LMud_Lisp* self, LMud_Any value);
LMud_Any LMud_Lisp_Denominator(struct LMud_Lisp* self, LMud_Any value);

bool LMud_Lisp_NumericEqual(struct LMud_Lisp* self, LMud_Any a, LMud_Any b);
bool LMud_Lisp_NumericLess(struct LMud_Lisp* self, LMud_Any a, LMud_Any b);
bool LMud_Lisp_NumericLessEqual(struct LMud_Lisp* self, LMud_Any a, LMud_Any b);
bool LMud_Lisp_NumericGreater(struct LMud_Lisp* self, LMud_Any a, LMud_Any b);
bool LMud_Lisp_NumericGreaterEqual(struct LMud_Lisp* self, LMud_Any a, LMud_Any b);

bool LMud_Lisp_Gcd(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result);
bool LMud_Lisp_RatioOrInteger(struct LMud_Lisp* self, LMud_Any numerator, LMud_Any denominator, LMud_Any* result);
bool LMud_Lisp_Add2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result);
bool LMud_Lisp_Sub2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result);
bool LMud_Lisp_Mul2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result);
bool LMud_Lisp_Div2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result);
bool LMud_Lisp_Mod2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result);

bool LMud_Lisp_Truncate(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result);

bool LMud_Lisp_LogAnd2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result);
bool LMud_Lisp_LogIor2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result);
bool LMud_Lisp_LogXor2(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result);

bool LMud_Lisp_LogNot(struct LMud_Lisp* self, LMud_Any a, LMud_Any* result);

bool LMud_Lisp_Ash(struct LMud_Lisp* self, LMud_Any a, LMud_Any b, LMud_Any* result);
