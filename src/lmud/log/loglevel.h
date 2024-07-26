/**
 * @file loglevel.h
 * @brief Logging Levels
 * 
 * This file contains the log levels used in the LMud project.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#pragma once

#include <lmud/defs.h>

enum LMud_LogLevel
{
    LMud_LogLevel_ALL,        // Extremely voluminous, includes full GC traces
    LMud_LogLevel_FULL_DEBUG, // Trace the entire execution flow, including events and periodic operations (i.e. the main loop)
    LMud_LogLevel_HALF_DEBUG, // Trace more fine-grained execution flow of the program, including incoming and outgoing events
    LMud_LogLevel_DEBUG,      // Trace general execution flow of the program (no events)
    LMud_LogLevel_NOTE,       // Mid-level subsystem messages (e.g. garbage collection)
    LMud_LogLevel_WARNING,    // Something went wrong - no consequences, full recovery
    LMud_LogLevel_ERROR,      // Something went wrong - request could not be completed
    LMud_LogLevel_INFO,       // High-level subsystem messages (e.g. loading a config file, opening a port, saving to disk, shutting down)
    LMud_LogLevel_FATAL       // Something went wrong - driver must shut down
};

const char* LMud_LogLevel_ToString(enum LMud_LogLevel loglevel);
const char* LMud_LogLevel_EscapeSequence(enum LMud_LogLevel loglevel);
