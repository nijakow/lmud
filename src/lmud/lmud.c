
#include <lmud/lisp/io.h>
#include <lmud/lisp/compiler/compiler.h>
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
    struct LMud_CompilerSession  session;
    struct LMud_Compiler         compiler;
    LMud_Any                     function;

    printf("; Compiling...\n");

    LMud_CompilerSession_Create(&session, &self->lisp);
    LMud_Compiler_Create(&compiler, &session);

    LMud_Compiler_Compile(&compiler, expression);

    function = LMud_Compiler_Build(&compiler);

    LMud_Compiler_Destroy(&compiler);
    LMud_CompilerSession_Destroy(&session);

    return function;
}

void LMud_TestRun(struct LMud* self, LMud_Any function)
{
    struct LMud_Scheduler*  scheduler;
    struct LMud_Fiber*      fiber;

    scheduler = &self->scheduler;
    fiber     = LMud_Scheduler_SpawnFiber(scheduler);

    printf("; Running...\n");

    LMud_Fiber_EnterThunk(fiber, function);
    LMud_Fiber_Tick(fiber);

    LMud_Fiber_Destroy(fiber);
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
        LMud_TestRun(self, value);
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
