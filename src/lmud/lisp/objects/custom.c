
#include <lmud/lisp/gc.h>
#include <lmud/util/memory.h>

#include "custom.h"

void LMud_Custom_Create(struct LMud_Custom* self, LMud_Any meta, LMud_Any* slots, LMud_Size size)
{
    LMud_Size  index;

    self->meta  = meta;
    self->size  = size;
    self->slots = LMud_Alloc(sizeof(LMud_Any) * size);

    for (index = 0; index < size; index++)
    {
        self->slots[index] = slots[index];
    }
}

void LMud_Custom_Destroy(struct LMud_Custom* self)
{
    LMud_Free(self->slots);
}

void LMud_Custom_Mark(struct LMud_GC* gc, struct LMud_Custom* self)
{
    LMud_Size  index;

    LMud_GC_MarkAny(gc, self->meta);

    for (index = 0; index < self->size; index++)
    {
        LMud_GC_MarkAny(gc, self->slots[index]);
    }
}

LMud_Any  LMud_Custom_Meta(struct LMud_Custom* self)
{
    return self->meta;
}

void LMud_Custom_SetMeta(struct LMud_Custom* self, LMud_Any meta)
{
    self->meta = meta;
}

LMud_Size LMud_Custom_Size(struct LMud_Custom* self)
{
    return self->size;
}

LMud_Any LMud_Custom_At(struct LMud_Custom* self, LMud_Size index)
{
    assert(index < self->size);
    return self->slots[index];
}

void LMud_Custom_Set(struct LMud_Custom* self, LMud_Size index, LMud_Any value)
{
    assert(index < self->size);
    self->slots[index] = value;
}
