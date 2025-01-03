
#pragma once

#include <lmud/lisp/base.h>

struct LMud_Cons
{
    LMud_Any            car;
    LMud_Any            cdr;
};

void LMud_Cons_Create(struct LMud_Cons* self, LMud_Any car, LMud_Any cdr);
void LMud_Cons_Destroy(struct LMud_Cons* self);
void LMud_Cons_Mark(struct LMud_GC* gc, struct LMud_Cons* self);
LMud_Size LMud_Cons_CalculateSizeInBytes(struct LMud_Cons* self);
