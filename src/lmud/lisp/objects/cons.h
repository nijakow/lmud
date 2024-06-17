
#pragma once

#include <lmud/lisp/lisp.h>

struct LMud_Cons
{
    struct LMud_Object  _;

    LMud_Any            car;
    LMud_Any            cdr;
};

void LMud_Cons_Create(struct LMud_Cons* self, LMud_Any car, LMud_Any cdr);
void LMud_Cons_Destroy(struct LMud_Cons* self);
