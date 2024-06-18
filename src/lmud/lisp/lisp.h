
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/lisp/objects.h>

struct LMud_Lisp
{
    struct LMud_Objects  objects;
};

bool LMud_Lisp_Create(struct LMud_Lisp* self);
void LMud_Lisp_Destroy(struct LMud_Lisp* self);
