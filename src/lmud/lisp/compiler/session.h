
#pragma once

#include <lmud/lisp/base.h>

struct LMud_CompilerSession
{
    struct LMud_Lisp*  lisp;
};

void LMud_CompilerSession_Create(struct LMud_CompilerSession* self, struct LMud_Lisp* lisp);
void LMud_CompilerSession_Destroy(struct LMud_CompilerSession* self);

struct LMud_Lisp* LMud_CompilerSession_GetLisp(struct LMud_CompilerSession* self);
