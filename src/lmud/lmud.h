/**
 * @file lmud.h
 * @brief The LMud Kernel Header
 * 
 * This file contains the root structure and functions for the LMud project.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#pragma once

#include <lmud/defs.h>
#include <lmud/log/log.h>
#include <lmud/net/net.h>
#include <lmud/lisp/lisp.h>
#include <lmud/lisp/runtime/scheduler.h>
#include <lmud/user/profile.h>
#include <lmud/util/stream.h>

struct LMud
{
    struct LMud_Log       log;
    struct LMud_Profiles  profiles;
    struct LMud_Net       net;
    struct LMud_Lisp      lisp;
    struct timeval        start_time;
    bool                  running;
};

bool LMud_Create(struct LMud* self);
void LMud_Destroy(struct LMud* self);

void LMud_Logf(struct LMud* self, enum LMud_LogLevel loglevel, const char* format, ...);

void LMud_SignalInterrupt(struct LMud* self, int signal);

void LMud_Main(struct LMud* self, int argc, char* argv[]);
