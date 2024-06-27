
#pragma once

#include <lmud/lisp/base.h>

struct LMud_GC
{

};

void LMud_GC_Create(struct LMud_GC* self);
void LMud_GC_Destroy(struct LMud_GC* self);
