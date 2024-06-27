
#include <lmud/lisp/io.h>
#include <lmud/lisp/runtime/fiber.h>

#include "lmud.h"


bool LMud_Create(struct LMud* self)
{
    self->running = true;
    return LMud_Net_Create(&self->net)
        && LMud_Lisp_Create(&self->lisp);
}

void LMud_Destroy(struct LMud* self)
{
    LMud_Lisp_Destroy(&self->lisp);
    LMud_Net_Destroy(&self->net);
}

void LMud_SignalInterrupt(struct LMud* self, int signal)
{
    (void) self;
    (void) signal;

    printf("\nInterrupted.\n");

    switch (signal)
    {
        case SIGINT:
        case SIGTERM:
            exit(0);
            break;
        default:
            break;
    }
}

void LMud_Tick(struct LMud* self)
{
    // LMud_Net_Tick(&self->net);
    LMud_Lisp_Tick(&self->lisp);
}

void LMud_Loop(struct LMud* self)
{
    while (self->running)
    {
        LMud_Tick(self);
    }
}

void LMud_Banner(struct LMud* self)
{
    (void) self;

    printf("\n");
    printf("  LMud v%s %s '%s'\n", LMud_VERSION, LMud_VERSION_EXTRA, LMud_RELEASE_NAME);
    printf("  Copyright (c) 2024 nijakow\n");
    printf("\n");
}

void LMud_Start(struct LMud* self)
{
    LMud_Any  boot_function;

    if (LMud_Lisp_LoadFile(&self->lisp, "../boot/prelude.lisp", &boot_function)) {
        LMud_Lisp_Kickstart(&self->lisp, boot_function);
    }
}

void LMud_Main(struct LMud* self, int argc, char* argv[])
{
    (void) argc;
    (void) argv;

    LMud_Banner(self);
    LMud_Start(self);
    LMud_Loop(self);
}
