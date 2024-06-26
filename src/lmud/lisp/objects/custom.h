
#pragma once

#include <lmud/lisp/base.h>

struct LMud_Custom
{
    struct LMud_Object  _;

    LMud_Any            meta;
    LMud_Size           size;
    LMud_Any*           slots;
};

void LMud_Custom_Create(struct LMud_Custom* self, LMud_Any meta, LMud_Any* slots, LMud_Size size);
void LMud_Custom_Destroy(struct LMud_Custom* self);

LMud_Any  LMud_Custom_Meta(struct LMud_Custom* self);
void      LMud_Custom_SetMeta(struct LMud_Custom* self, LMud_Any meta);

LMud_Size LMud_Custom_Size(struct LMud_Custom* self);
LMud_Any  LMud_Custom_At(struct LMud_Custom* self, LMud_Size index);
void      LMud_Custom_Set(struct LMud_Custom* self, LMud_Size index, LMud_Any value);
