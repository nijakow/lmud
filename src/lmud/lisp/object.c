
#include <lmud/lisp/objects.h>
#include <lmud/lisp/gc.h>

#include "object.h"


bool LMud_Header_Create(struct LMud_Header* self, struct LMud_Objects* objects, struct LMud_Type* type)
{
    (void) objects;

    self->next       = objects->objects;
    objects->objects = self;

    self->type = type;

    LMud_Header_SetLink(self, NULL);
    LMud_Header_SetGCBits(self, LMud_GCBits_White);

    return true;
}

void LMud_Header_Destroy(struct LMud_Header* self)
{
    (void) self;
}

struct LMud_Header* LMud_ToHeader(void* object)
{
    return (struct LMud_Header*) object - 1;
}

void* LMud_Header_ToObject(struct LMud_Header* header)
{
    return header + 1;
}


struct LMud_Header* LMud_Header_GetLink(struct LMud_Header* self)
{
    return (struct LMud_Header*) (((uintptr_t) self->link) & ~((uintptr_t) 0x03));
}

void LMud_Header_SetLink(struct LMud_Header* self, struct LMud_Header* link)
{
    self->link = (struct LMud_Header*) (((uintptr_t) link) | (((uintptr_t) self->link) & 0x03));
}

enum LMud_GCBits LMud_Header_GetGCBits(struct LMud_Header* self)
{
    return (enum LMud_GCBits) (((uintptr_t) self->link) & 0x03);
}

void LMud_Header_SetGCBits(struct LMud_Header* self, enum LMud_GCBits gc_bits)
{
    self->link = (struct LMud_Header*) ((((uintptr_t) self->link) & ~((uintptr_t) 0x03)) | ((uintptr_t) gc_bits));
}


bool LMud_Type_TypeCheckHeader(struct LMud_Type* self, struct LMud_Header* header)
{
    return header->type == self;
}

bool LMud_Type_TypeCheckObject(struct LMud_Type* self, void* object)
{
    return LMud_Type_TypeCheckHeader(self, LMud_ToHeader(object));
}
