
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
