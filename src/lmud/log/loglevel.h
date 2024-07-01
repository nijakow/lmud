
#pragma once

#include <lmud/defs.h>

enum LMud_LogLevel
{
    LMud_LogLevel_FULL_DEBUG,
    LMud_LogLevel_HALF_DEBUG,
    LMud_LogLevel_DEBUG,
    LMud_LogLevel_NOTE,
    LMud_LogLevel_WARNING,
    LMud_LogLevel_ERROR,
    LMud_LogLevel_INFO,
    LMud_LogLevel_FATAL
};

const char* LMud_LogLevel_ToString(enum LMud_LogLevel loglevel);
const char* LMud_LogLevel_EscapeSequence(enum LMud_LogLevel loglevel);
