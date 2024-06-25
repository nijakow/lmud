
#pragma once

#include <lmud/lisp/base.h>

struct LMud_Ratio
{
    struct LMud_Object  _;

    LMud_Any            numerator;
    LMud_Any            denominator;
};

void LMud_Ratio_Create(struct LMud_Ratio* self, LMud_Any numerator, LMud_Any denominator);
void LMud_Ratio_Destroy(struct LMud_Ratio* self);

LMud_Any LMud_Ratio_Numerator(struct LMud_Ratio* self);
LMud_Any LMud_Ratio_Denominator(struct LMud_Ratio* self);
