
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
    struct LMud_Lisp*        lisp;
    struct LMud_InputStream  stream;

    lisp = &self->lisp;

    LMud_InputStream_CreateFromFile(&stream, stdin);

    while (true)
    {
        LMud_Lisp_Print(lisp, LMud_Lisp_Read(lisp, &stream), stdout, true);
        putchar('\n');
    }
}

void LMud_Main(struct LMud* self, int argc, char* argv[])
{
    (void) argc;
    (void) argv;

    LMud_Test(self);
}
