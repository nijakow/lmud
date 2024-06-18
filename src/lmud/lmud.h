
#pragma once

#include <lmud/defs.h>
#include <lmud/lisp/lisp.h>

struct LMud
{
    struct LMud_Lisp  lisp;
};

bool LMud_Create(struct LMud* self);
void LMud_Destroy(struct LMud* self);

void LMud_Main(struct LMud* self, int argc, char* argv[]);
