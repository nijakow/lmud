
#include <lmud/lisp/io.h>
#include <lmud/lisp/runtime/fiber.h>

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
    printf("  LMud v%s %s '%s'\n", LMud_VERSION, LMud_VERSION_EXTRA, LMud_RELEASE_NAME);
    printf("  Copyright (c) 2024 nijakow\n");
    printf("\n");
}

LMud_Any LMud_TestCompile(struct LMud* self, LMud_Any expression)
{
    LMud_Any                     function;

    LMud_Lisp_Compile(&self->lisp, expression, &function);

    return function;
}

LMud_Any LMud_TestRun(struct LMud* self, LMud_Any function)
{
    LMud_Any  result;

    LMud_Scheduler_BlockAndRunThunk(&self->lisp.scheduler, function, &result);

    return result;
}

void LMud_Test(struct LMud* self)
{
    struct LMud_Lisp*        lisp;

    lisp = &self->lisp;

    LMud_Lisp_LoadFile(lisp, "../boot/prelude.lisp");
}

void LMud_Main(struct LMud* self, int argc, char* argv[])
{
    (void) argc;
    (void) argv;

    LMud_Banner(self);
    LMud_Test(self);
}
