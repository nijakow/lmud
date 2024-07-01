
#pragma once

#include <lmud/defs.h>
#include <lmud/log/loglevel.h>


struct LMud;
enum   LMud_LogLevel;
struct LMud_Lisp;
struct LMud_Header;
struct LMud_Objects;
struct LMud_Fiber;
struct LMud_Frame;
struct LMud_GC;
struct LMud_GCStats;
struct LMud_Net;


void LMud_Logf(struct LMud* lmud, enum LMud_LogLevel loglevel, const char* format, ...);
