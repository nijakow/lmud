
#include <lmud/glue.h>
#include <lmud/log/log.h>
#include <lmud/lisp/io.h>
#include <lmud/lisp/runtime/fiber.h>

#include "lmud.h"


bool LMud_Create(struct LMud* self)
{
    self->running = true;
    gettimeofday(&self->start_time, NULL);
    return LMud_Log_Create(&self->log)
        && LMud_Profiles_Create(&self->profiles)
        && LMud_Net_Create(&self->net, self)
        && LMud_Lisp_Create(&self->lisp, self);
}

void LMud_Destroy(struct LMud* self)
{
    LMud_Lisp_Destroy(&self->lisp);
    LMud_Net_Destroy(&self->net);
    LMud_Log_Destroy(&self->log);
}


void LMud_SignalInterrupt(struct LMud* self, int signal)
{
    LMud_Logf(self, LMud_LogLevel_NOTE, "Caught signal %d", signal);

    switch (signal)
    {
        case SIGINT:
        case SIGTERM:
            LMud_Logf(self, LMud_LogLevel_FATAL, "Terminating!");
            exit(0);
            break;
        default:
            LMud_Debugf(self, LMud_LogLevel_DEBUG, "Handling signal %d --> ignored!", signal);
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

void LMud_StartupInfo(struct LMud* self)
{
    LMud_Banner(self);

    LMud_Logf(self, LMud_LogLevel_INFO, "Starting up LMud v%s %s '%s'...", LMud_VERSION, LMud_VERSION_EXTRA, LMud_RELEASE_NAME);

    {
        struct LMud_LogComposer   composer;
        struct LMud_OutputStream  stream;

        LMud_LogComposer_Create(&composer, &self->log, LMud_LogLevel_NOTE);
        LMud_OutputStream_CreateOnLogComposer(&stream, &composer);
        {
            LMud_OutputStream_Printf(&stream, "Compiled with the following properties:\n");
            LMud_OutputStream_Printf(&stream, "  - Hardcoded Log Level:       %s\n", LMud_LogLevel_ToString(LMud_HARDCODED_LOG_LEVEL));
            LMud_OutputStream_Printf(
                &stream,
                "  - Compressed LMud_Any:       %s\n",
#ifdef LMud_ENABLE_COMPRESSED_ANYS
                "true"
#else
                "false"
#endif
            );
            LMud_OutputStream_Printf(
                &stream,
                "  - Support for malloc_trim(): %s\n",
#ifdef LMud_ENABLE_MALLOC_TRIM
                "true"
#else
                "false"
#endif
            );
        }
        LMud_OutputStream_Destroy(&stream);
        LMud_LogComposer_Commit(&composer);
        LMud_LogComposer_Destroy(&composer);
    }
}

void LMud_Startup(struct LMud* self)
{
    struct LMud_Profile*  system_profile;
    LMud_Any              boot_function;

    system_profile = LMud_Profiles_GetSystemProfile(&self->profiles);

    LMud_StartupInfo(self);

    if (LMud_Lisp_LoadFile(&self->lisp, system_profile, "../boot/prelude.lisp", &boot_function))
    {
        LMud_Lisp_Kickstart(&self->lisp, system_profile, boot_function);
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

    LMud_Startup(self);
    LMud_Loop(self);
    LMud_Shutdown(self);
}
