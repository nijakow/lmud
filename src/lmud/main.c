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

struct LMud  LMUD;


static void LMud_SignalHandler(int signal)
{
    LMud_SignalInterrupt(&LMUD, signal);
}

static void LMud_InstallSignalHandlers()
{
    signal(SIGINT,  LMud_SignalHandler);
    signal(SIGTERM, LMud_SignalHandler);
}

static void LMud_Prepare()
{
    srand(time(NULL));
    LMud_InstallSignalHandlers();
}


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
