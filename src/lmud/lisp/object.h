
#pragma once

#include <lmud/defs.h>

struct LMud_Header
{
};

struct LMud_Object
{
    struct LMud_Object*  next;
    struct LMud_Header   header;
};

bool LMud_Object_Create(struct LMud_Object* self);
void LMud_Object_Destroy(struct LMud_Object* self);
