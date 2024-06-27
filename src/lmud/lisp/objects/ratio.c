
#include <lmud/lisp/gc.h>

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

void LMud_Ratio_Mark(struct LMud_GC* gc, struct LMud_Ratio* self)
{
    LMud_GC_MarkAny(gc, self->numerator);
    LMud_GC_MarkAny(gc, self->denominator);
}

LMud_Size LMud_Ratio_CalculateSizeInBytes(struct LMud_Ratio* self)
{
    (void) self;
    return sizeof(struct LMud_Ratio);
}


LMud_Any LMud_Ratio_Numerator(struct LMud_Ratio* self)
{
    return self->numerator;
}

LMud_Any LMud_Ratio_Denominator(struct LMud_Ratio* self)
{
    return self->denominator;
}
