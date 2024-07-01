
#include <lmud/lisp/io.h>
#include <lmud/lisp/runtime/fiber.h>

#include "lmud.h"


bool LMud_Create(struct LMud* self)
{
    self->running = true;
    return LMud_Log_Create(&self->log)
        && LMud_Net_Create(&self->net, self)
        && LMud_Lisp_Create(&self->lisp, self);
}

void LMud_Destroy(struct LMud* self)
{
    LMud_Lisp_Destroy(&self->lisp);
    LMud_Net_Destroy(&self->net);
    LMud_Log_Destroy(&self->log);
}


struct LMud_Log* LMud_GetLog(struct LMud* self)
{
    return &self->log;
}

struct LMud_Lisp* LMud_GetLisp(struct LMud* self)
{
    return &self->lisp;
}

struct LMud_Net* LMud_GetNet(struct LMud* self)
{
    return &self->net;
}

void LMud_Logf(struct LMud* mud, enum LMud_LogLevel loglevel, const char* format, ...)
{
    va_list                  args;
    struct LMud_LogComposer  composer;
    struct LMud_OutputStream stream;

    va_start(args, format);
    {
        LMud_LogComposer_Create(&composer, LMud_GetLog(mud), loglevel);
        LMud_OutputStream_CreateOnLogComposer(&stream, &composer);
        LMud_OutputStream_VPrintf(&stream, format, args);
        LMud_OutputStream_Destroy(&stream);
        LMud_LogComposer_Commit(&composer);
        LMud_LogComposer_Destroy(&composer);
    }
    va_end(args);
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
    LMud_Net_Tick(&self->net, !LMud_Lisp_NeedsControlBackImmediately(&self->lisp));
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

void LMud_Startup(struct LMud* self)
{
    LMud_Any  boot_function;

    LMud_Logf(self, LMud_LogLevel_INFO, "Starting up LMud v%s %s '%s'...\n", LMud_VERSION, LMud_VERSION_EXTRA, LMud_RELEASE_NAME);

    if (LMud_Lisp_LoadFile(&self->lisp, "../boot/prelude.lisp", &boot_function))
    {
        LMud_Lisp_Kickstart(&self->lisp, boot_function);
    }
}

void LMud_Shutdown(struct LMud* self)
{
    LMud_Logf(self, LMud_LogLevel_INFO, "Shutting down...\n");
}

void LMud_Main(struct LMud* self, int argc, char* argv[])
{
    (void) argc;
    (void) argv;

    LMud_Banner(self);
    LMud_Startup(self);
    LMud_Loop(self);
    LMud_Shutdown(self);
}
