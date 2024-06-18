
#include "lmud.h"


bool LMud_Create(struct LMud* self)
{
    return LMud_Lisp_Create(&self->lisp);
}

void LMud_Destroy(struct LMud* self)
{
    LMud_Lisp_Destroy(&self->lisp);
}

void LMud_Main(struct LMud* self, int argc, char* argv[])
{
    (void) self;
    (void) argc;
    (void) argv;
}
