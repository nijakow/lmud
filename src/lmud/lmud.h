
#pragma once

#include <lmud/defs.h>
#include <lmud/log/log.h>
#include <lmud/net/net.h>
#include <lmud/lisp/lisp.h>
#include <lmud/lisp/runtime/scheduler.h>
#include <lmud/util/stream.h>

struct LMud
{
    struct LMud_Log   log;
    struct LMud_Net   net;
    struct LMud_Lisp  lisp;
    bool              running;
};

bool LMud_Create(struct LMud* self);
void LMud_Destroy(struct LMud* self);

void LMud_Logf(struct LMud* self, enum LMud_LogLevel loglevel, const char* format, ...);

void LMud_SignalInterrupt(struct LMud* self, int signal);

void LMud_Main(struct LMud* self, int argc, char* argv[]);
