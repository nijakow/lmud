
#include <lmud/lisp/io.h>
#include <lmud/lisp/runtime/fiber.h>

#include "lmud.h"


bool LMud_Create(struct LMud* self)
{
    return LMud_Lisp_Create(&self->lisp)
        && LMud_Scheduler_Create(&self->scheduler, &self->lisp);
}

void LMud_Destroy(struct LMud* self)
{
    LMud_Scheduler_Destroy(&self->scheduler);
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
    struct LMud_Scheduler*  scheduler;
    struct LMud_Fiber*      fiber;
    LMud_Any                result;

    scheduler = &self->scheduler;
    fiber     = LMud_Scheduler_SpawnFiber(scheduler);

    LMud_Fiber_EnterThunk(fiber, function);
    LMud_Fiber_Tick(fiber);

    result = LMud_Fiber_GetAccumulator(fiber);

    return result;
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
        value = LMud_TestCompile(self, value);
        value = LMud_TestRun(self, value);
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
