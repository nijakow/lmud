
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

void LMud_Banner(struct LMud* self)
{
    (void) self;

    printf("\n");
    printf("  LMud v%s\n", LMud_VERSION);
    printf("  Copyright (c) 2024 nijakow\n");
}

void LMud_Test(struct LMud* self)
{
    struct LMud_Lisp*        lisp;
    struct LMud_InputStream  stream;
    LMud_Any                 value;

    lisp = &self->lisp;

    LMud_InputStream_CreateFromFile(&stream, stdin);

    while (!LMud_InputStream_Eof(&stream))
    {
        printf("> ");
        fflush(stdout);
        value = LMud_Lisp_Read(lisp, &stream);
        printf("  ");
        LMud_Lisp_Print(lisp, value, stdout, true);
        putchar('\n');
    }
}

void LMud_Main(struct LMud* self, int argc, char* argv[])
{
    (void) argc;
    (void) argv;

    LMud_Banner(self);
    LMud_Test(self);
}
