
#pragma once

#include <lmud/defs.h>


typedef LMud_Size (*LMud_SizeFunc)(void* object);
typedef void      (*LMud_MarkFunc)(struct LMud_GC* gc, void* object);
typedef void      (*LMud_Destructor)(void* object);

struct LMud_Type
{
    const char*      name;
    LMud_Size        base_size;
    LMud_SizeFunc    size_func;
    LMud_MarkFunc    marker;
    LMud_Destructor  destructor;
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

struct LMud_Header* LMud_Header_GetLink(struct LMud_Header* self);
void                LMud_Header_SetLink(struct LMud_Header* self, struct LMud_Header* link);
enum LMud_GCBits    LMud_Header_GetGCBits(struct LMud_Header* self);
void                LMud_Header_SetGCBits(struct LMud_Header* self, enum LMud_GCBits gc_bits);

bool LMud_Type_TypeCheckHeader(struct LMud_Type* self, struct LMud_Header* header);
bool LMud_Type_TypeCheckObject(struct LMud_Type* self, void* object);
