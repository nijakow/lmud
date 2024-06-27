
#pragma once

#include <lmud/defs.h>
#include <lmud/net/net.h>
#include <lmud/lisp/lisp.h>
#include <lmud/lisp/runtime/scheduler.h>

struct LMud
{
    struct LMud_Net   net;
    struct LMud_Lisp  lisp;
};

bool LMud_Create(struct LMud* self);
void LMud_Destroy(struct LMud* self);

void LMud_SignalInterrupt(struct LMud* self, int signal);

void LMud_Main(struct LMud* self, int argc, char* argv[]);
