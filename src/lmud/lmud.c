/**
 * @file lmud.c
 * @brief The LMud Kernel
 * 
 * This file contains the root structure and functions for the LMud project.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#include <lmud/glue.h>
#include <lmud/lisp/io.h>
#include <lmud/lisp/runtime/fiber.h>
#include <lmud/sys/log/log.h>

#include "lmud.h"


/**
 * @brief Create an LMud Kernel.
 * 
 * This function creates an LMud Kernel and initializes all
 * submodules.
 * 
 * @param self The LMud Kernel
 * 
 * @return `true` if the LMud Kernel was created successfully, `false` otherwise
 */
bool LMud_Create(struct LMud* self)
{
    /*
     * Nil fieri ex nihilo, in nihilum nil posse reverti.
     *                                  - Lucretius
     * 
     * Nothing can be created from nothing, and nothing can be returned to nothing.
     *                                                    - Lucretius
     */

    self->running = true;
    gettimeofday(&self->start_time, NULL);
    return LMud_Log_Create(&self->log)
        && LMud_Console_Create(&self->console, self)
        && LMud_Profiles_Create(&self->profiles)
        && LMud_Net_Create(&self->net, self)
        && LMud_Lisp_Create(&self->lisp, self);
}

/**
 * @brief Destroy an LMud Kernel.
 * 
 * This function destroys an LMud Kernel and all submodules.
 * 
 * @param self The LMud Kernel
 */
void LMud_Destroy(struct LMud* self)
{
    /*
     * Μηδὲν ἄγαν.
     *      - Χίλων
     * 
     * Nothing in excess.
     *     - Chilon
     */

    LMud_Lisp_Destroy(&self->lisp);
    LMud_Net_Destroy(&self->net);
    LMud_Profiles_Destroy(&self->profiles);
    LMud_Console_Destroy(&self->console);
    LMud_Log_Destroy(&self->log);
}


/**
 * @brief Signal the LMud Kernel that an interrupt has occurred.
 * 
 * This function is called when a signal is caught by the C runtime,
 * and it forwards the signal to the LMud Kernel.
 * 
 * Depending on the signal, the LMud Kernel might shut down or
 * ignore the signal.
 */
void LMud_SignalInterrupt(struct LMud* self, int signal)
{
    /*
     * μή μου τοὺς κύκλους τάραττε!
     *              - Ἀρχιμήδης
     * 
     * Do not disturb my circles!
     *             - Archimedes
     */

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

/**
 * @brief The main tick function of the LMud Kernel.
 * 
 * This function is the main tick function of the LMud Kernel.
 * 
 * It calls the tick functions in the correct order and ensures
 * that the LMud Kernel is running smoothly without any blocking
 * calls.
 * 
 * @param self The LMud Kernel
 */
void LMud_Tick(struct LMud* self)
{
    LMud_Net_Tick(&self->net, !LMud_Lisp_NeedsControlBackImmediately(&self->lisp));
    LMud_Lisp_Tick(&self->lisp);
    LMud_Console_Tick(&self->console);
}

/**
 * @brief The main loop of the LMud Kernel.
 * 
 * This function is the main loop of the LMud Kernel.
 * 
 * It runs until the kernel is told to stop through an interrupt
 * and continuously calls the `LMud_Tick` function.
 * 
 * @param self The LMud Kernel
 */
void LMud_Loop(struct LMud* self)
{
    /*
     * The millstones of the gods grind late, but they grind fine.
     *                                      - Ancient Proverb
     */
    while (self->running)
    {
        LMud_Tick(self);
    }
}

/**
 * @brief Print the LMud banner.
 * 
 * This function prints the LMud banner to the console, which is
 * usually displayed when the LMud Kernel starts up.
 * 
 * @param self The LMud Kernel
 */
void LMud_Banner(struct LMud* self)
{
    (void) self;

    printf("\n");
    printf("  LMud v%s-%s '%s'\n", LMud_VERSION, LMud_VERSION_EXTRA, LMud_RELEASE_NAME);
    printf("  Copyright (c) 2024 Eric Felix Nijakowski\n");
    printf("\n");
}

/**
 * @brief Provide some information about the LMud Kernel in the log.
 * 
 * This function logs some information about the LMud Kernel
 * via the default log system.
 * 
 * The information includes the version, release name, and some compile-time
 * properties and build options.
 * 
 * @param self The LMud Kernel
 */
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

/**
 * @brief Boot an LMud kernel with the prelude.
 * 
 * This function is responsible for booting the LMud kernel
 * by loading the `prelude.lisp` file and starting the boot
 * function to provide us with a proper Lisp environment.
 * 
 * @param self The LMud Kernel
 */
void LMud_Startup(struct LMud* self)
{
    struct LMud_Profile*  system_profile;
    LMud_Any              boot_function;

    system_profile = LMud_Profiles_GetSystemProfile(&self->profiles);

    LMud_StartupInfo(self);

    if (LMud_Lisp_LoadFile(&self->lisp, system_profile, "../lisp/prelude.lisp", &boot_function))
    {
        LMud_Lisp_Kickstart(&self->lisp, system_profile, boot_function);
    }
}

/**
 * @brief Shut down the LMud kernel.
 * 
 * This function brings the kernel to a graceful stop.
 * 
 * At the moment, we just log a message and return.
 * 
 * @param self The LMud Kernel
 */
void LMud_Shutdown(struct LMud* self)
{
    LMud_Logf(self, LMud_LogLevel_INFO, "Shutting down...\n");
}

/**
 * @brief The main entry point for the LMud project.
 * 
 * This function gets called directly from `main` and
 * is the actual main function of this project.
 * 
 * @param self The LMud Kernel
 * @param argc The number of command line arguments
 * @param argv The command line arguments
 */
void LMud_Main(struct LMud* self, int argc, char* argv[])
{
    /*
     * ὁδὸς ἄνω κάτω μία καὶ ὡυτή
     *             - Ἡράκλειτος
     *
     * The way up and the way down are one and the same.
     *                                - Heraclitus
     */

    (void) argc;
    (void) argv;

    /*
     * Start up the LMud Kernel with the mudlib,
     * run the main loop, and shut down after everything
     * is done.
     */
    LMud_Startup(self);
    LMud_Loop(self);
    LMud_Shutdown(self);
}
