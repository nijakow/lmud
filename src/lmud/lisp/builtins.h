
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/lisp/lisp.h>

#define LMud_VARIADIC_ARGS ((LMud_Size) -1)

bool LMud_Fiber_CheckArgs(struct LMud_Fiber* fiber, LMud_Any* args, LMud_Size size, LMud_Size min, LMud_Size max, const char* func, const char* file, unsigned int line);

void LMud_Lisp_InstallBuiltins(struct LMud_Lisp* lisp);
