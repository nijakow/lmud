/**
 * @file loglevel.c
 * @brief Logging Levels
 * 
 * This file contains the log levels for the LMud project.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#include "loglevel.h"

const char* LMud_LogLevel_ToString(enum LMud_LogLevel loglevel)
{
    switch (loglevel)
    {
        case LMud_LogLevel_ALL:        return "   *   ";
        case LMud_LogLevel_FULL_DEBUG: return "DEBUG*";
        case LMud_LogLevel_HALF_DEBUG: return "DEBUG+";
        case LMud_LogLevel_DEBUG:      return "DEBUG";
        case LMud_LogLevel_NOTE:       return "NOTE";
        case LMud_LogLevel_WARNING:    return "WARNING";
        case LMud_LogLevel_ERROR:      return "ERROR";
        case LMud_LogLevel_INFO:       return "INFO";
        case LMud_LogLevel_FATAL:      return "FATAL";
        default:                       return "UNKNOWN";
    }
}

const char* LMud_LogLevel_EscapeSequence(enum LMud_LogLevel loglevel)
{
    switch (loglevel)
    {
        case LMud_LogLevel_ALL:        return "\033[1;34m";
        case LMud_LogLevel_FULL_DEBUG: return "\033[1;34m";
        case LMud_LogLevel_HALF_DEBUG: return "\033[1;34m";
        case LMud_LogLevel_DEBUG:      return "\033[1;34m";
        case LMud_LogLevel_NOTE:       return "\033[1;30m";
        case LMud_LogLevel_WARNING:    return "\033[1;33m";
        case LMud_LogLevel_ERROR:      return "\033[1;31m";
        case LMud_LogLevel_INFO:       return "\033[1;32m";
        case LMud_LogLevel_FATAL:      return "\033[1;35m";
        default:                       return "\033[1;37m";
    }
}
