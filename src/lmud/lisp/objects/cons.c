
#include <lmud/lisp/gc.h>

#include "cons.h"

void LMud_Cons_Create(struct LMud_Cons* self, LMud_Any car, LMud_Any cdr)
{
    self->car = car;
    self->cdr = cdr;
}

void LMud_Cons_Destroy(struct LMud_Cons* self)
{
    (void) self;
}

void LMud_Cons_Mark(struct LMud_GC* gc, struct LMud_Cons* self)
{
    LMud_GC_MarkAny(gc, self->car);
    LMud_GC_MarkAny(gc, self->cdr);
}

LMud_Size LMud_Cons_CalculateSizeInBytes(struct LMud_Cons* self)
{
    (void) self;
    return sizeof(struct LMud_Cons);
}
