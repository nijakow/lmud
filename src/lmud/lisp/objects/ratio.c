
#include "ratio.h"

void LMud_Ratio_Create(struct LMud_Ratio* self, LMud_Any numerator, LMud_Any denominator)
{
    self->numerator   = numerator;
    self->denominator = denominator;
}

void LMud_Ratio_Destroy(struct LMud_Ratio* self)
{
    (void) self;
}

LMud_Any LMud_Ratio_Numerator(struct LMud_Ratio* self)
{
    return self->numerator;
}

LMud_Any LMud_Ratio_Denominator(struct LMud_Ratio* self)
{
    return self->denominator;
}
