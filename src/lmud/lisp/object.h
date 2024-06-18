
#pragma once

#include <lmud/defs.h>


struct LMud_Type
{
    LMud_Size  base_size;
};

struct LMud_Header
{
    struct LMud_Type*  type;
};

struct LMud_Object
{
    struct LMud_Object*  next;
    struct LMud_Header   header;
};

bool LMud_Object_Create(struct LMud_Object* self, struct LMud_Objects* objects, struct LMud_Type* type);
void LMud_Object_Destroy(struct LMud_Object* self);


typedef void (*LMud_ObjectConstructor)(void*);
