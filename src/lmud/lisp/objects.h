
#pragma once

#include <lmud/lisp/object.h>

struct LMud_Objects
{
    struct LMud_Object*  objects;
};

void LMud_Objects_Create(struct LMud_Objects* self);
void LMud_Objects_Destroy(struct LMud_Objects* self);
