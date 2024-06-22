
#pragma once

#include <lmud/lisp/base.h>

struct LMud_CompilerSession
{
};

void LMud_CompilerSession_Create(struct LMud_CompilerSession* self);
void LMud_CompilerSession_Destroy(struct LMud_CompilerSession* self);
