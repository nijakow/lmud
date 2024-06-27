
#pragma once

#include <lmud/defs.h>


struct LMud_Type
{
    LMud_Size  base_size;
};

struct LMud_Header
{
    struct LMud_Header*  next;
    struct LMud_Header*  link;
    struct LMud_Type*    type;
};

bool LMud_Header_Create(struct LMud_Header* self, struct LMud_Objects* objects, struct LMud_Type* type);
void LMud_Header_Destroy(struct LMud_Header* self);

struct LMud_Header* LMud_ToHeader(void* object);
void*               LMud_Header_ToObject(struct LMud_Header* header);

bool LMud_Type_TypeCheckHeader(struct LMud_Type* self, struct LMud_Header* header);
bool LMud_Type_TypeCheckObject(struct LMud_Type* self, void* object);
