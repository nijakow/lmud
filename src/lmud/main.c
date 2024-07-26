/**
 * @file main.c
 * @brief LMud Entry Point
 * 
 * This file contains the `main` function for the LMud project,
 * as well as the signal handlers and the global variables.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#include <lmud/lmud.h>

/**
 * @brief The LMud instance.
 * 
 * This is the global LMud instance that is used by the `main` function.
 * 
 * The entire system state is stored in this instance.
 */
struct LMud  LMUD;


/**
 * @brief The signal handler for the LMud project.
 * 
 * @param signal The signal number, as defined in `signal.h`.
 * 
 * This function is the main signal handler for the LMud project.
 * It forwards all intercepted signals to the `LMud_SignalInterrupt`
 * method of the LMud instance.
 */
static void LMud_SignalHandler(int signal)
{
    LMud_SignalInterrupt(&LMUD, signal);
}

/**
 * @brief Install the signal handlers.
 * 
 * This function installs the signal handlers for the LMud project.
 * 
 * By default, we use a common interrupt handler (`LMud_SignalHandler`)
 * for all signals.
 */
static void LMud_InstallSignalHandlers()
{
    signal(SIGINT,  LMud_SignalHandler);
    signal(SIGTERM, LMud_SignalHandler);
}

/**
 * @brief Initialize the C runtime.
 * 
 * This function initializes the C runtime by setting the random seed
 * and installing the signal handlers.
 * 
 * Everything that is unrelated to the server instance is done here.
 */
static void LMud_Prepare()
{
    srand(time(NULL));
    LMud_InstallSignalHandlers();
}


/**
 * @brief The entry point for the LMud project.
 * 
 * @param argc The number of command line arguments.
 * @param argv The command line arguments.
 * 
 * @return The exit code. `0` by default.
 */
int main(int argc, char* argv[])
{
    if (LMud_Create(&LMUD))
    {
        LMud_Prepare();
        LMud_Main(&LMUD, argc, argv);
        LMud_Destroy(&LMUD);
    }
    return 0;
}
