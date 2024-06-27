
#pragma once

#include <lmud/lisp/base.h>


enum LMud_GCBits
{
    LMud_GCBits_White,
    LMud_GCBits_Grey,
    LMud_GCBits_Black,
};


struct LMud_GCStats
{
    LMud_Size  objects_freed;
    LMud_Size  objects_kept;
};

void LMud_GCStats_Create(struct LMud_GCStats* self);
void LMud_GCStats_Destroy(struct LMud_GCStats* self);


struct LMud_GC
{
    struct LMud_Header*  pending;
    struct LMud_Lisp*    lisp;

    struct LMud_GCStats  stats;
};

void LMud_GC_Create(struct LMud_GC* self, struct LMud_Lisp* lisp);
void LMud_GC_Destroy(struct LMud_GC* self);

void LMud_GC_FetchStats(struct LMud_GC* self, struct LMud_GCStats* stats);

void LMud_GC_MarkAny(struct LMud_GC* self, LMud_Any any);
void LMud_GC_MarkObject(struct LMud_GC* self, void* object);
void LMud_GC_MarkFrame(struct LMud_GC* self, struct LMud_Frame* frame);

void LMud_GC_Run(struct LMud_GC* self);
