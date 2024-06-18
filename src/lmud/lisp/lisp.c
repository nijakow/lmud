
#include "lisp.h"


bool LMud_Lisp_Create(struct LMud_Lisp* self)
{
    return LMud_Objects_Create(&self->objects);
}

void LMud_Lisp_Destroy(struct LMud_Lisp* self)
{
    LMud_Objects_Destroy(&self->objects);
}
