
#pragma once

#include <lmud/lisp/base.h>


enum LMud_GCBits
{
    LMud_GCBits_White,
    LMud_GCBits_Grey,
    LMud_GCBits_Black,
};


struct LMud_GC
{
    struct LMud_Header*  pending;
    struct LMud_Lisp*    lisp;
};

void LMud_GC_Create(struct LMud_GC* self, struct LMud_Lisp* lisp);
void LMud_GC_Destroy(struct LMud_GC* self);

void LMud_GC_MarkAny(struct LMud_GC* self, struct LMud_Any any);
void LMud_GC_MarkObject(struct LMud_GC* self, void* object);

void LMud_GC_Run(struct LMud_GC* self);
