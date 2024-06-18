
#include <lmud/lisp/io.h>

#include "lmud.h"


bool LMud_Create(struct LMud* self)
{
    return LMud_Lisp_Create(&self->lisp);
}

void LMud_Destroy(struct LMud* self)
{
    LMud_Lisp_Destroy(&self->lisp);
}

void LMud_Test(struct LMud* self)
{
    struct LMud_Lisp*  lisp;

    lisp = &self->lisp;

    LMud_Lisp_Print(lisp, LMud_Lisp_Cons(lisp, LMud_Lisp_Intern(lisp, "foo"), LMud_Lisp_Cons(lisp, LMud_Lisp_Intern(lisp, "bar"), LMud_Lisp_Intern(lisp, "baz"))), stdout, true);
}

void LMud_Main(struct LMud* self, int argc, char* argv[])
{
    (void) argc;
    (void) argv;

    LMud_Test(self);
}
