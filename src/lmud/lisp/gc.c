
#include <lmud/glue.h>
#include <lmud/lisp/lisp.h>
#include <lmud/lisp/objects.h>
#include <lmud/util/memory.h>

#include "gc.h"


void LMud_GCStats_Create(struct LMud_GCStats* self)
{
    self->objects_freed = 0;
    self->objects_kept  = 0;
}

void LMud_GCStats_Destroy(struct LMud_GCStats* self)
{
    (void) self;
}


void LMud_GC_Create(struct LMud_GC* self, struct LMud_Lisp* lisp)
{
    self->pending = NULL;
    self->lisp    = lisp;

    LMud_GCStats_Create(&self->stats);
}

void LMud_GC_Destroy(struct LMud_GC* self)
{
    assert(self->pending == NULL);
    LMud_GCStats_Destroy(&self->stats);
}

void LMud_GC_FetchStats(struct LMud_GC* self, struct LMud_GCStats* stats)
{
    if (stats != NULL)
    {
        *stats = self->stats;
    }
}

void LMud_GC_MarkAny(struct LMud_GC* self, LMud_Any any)
{
    if (LMud_Any_IsPointer(any))
    {
        LMud_GC_MarkObject(self, LMud_Any_AsPointer(any));
    }
}

void LMud_GC_MarkObject(struct LMud_GC* self, void* object)
{
    struct LMud_Header*  header;

    header = LMud_ToHeader(object);

    if (LMud_Header_GetGCBits(header) == LMud_GCBits_White)
    {
        LMud_Header_SetGCBits(header, LMud_GCBits_Grey);
        LMud_Header_SetLink(header, self->pending);
        self->pending = header;
    }
}

void LMud_GC_MarkFrame(struct LMud_GC* self, struct LMud_Frame* frame)
{
    LMud_Size  size;
    LMud_Size  index;

    /*
     * TODO, FIXME, XXX: This might cause a crash if lexical frames still
     *                   have their previous pointers set to a frame that
     *                   used to be on the stack but has been deallocated.
     */

    while (frame != NULL)
    {
        size = LMud_Frame_PayloadSizeInAnys(frame);

        for (index = 0; index < size; index++)
        {
            LMud_GC_MarkAny(self, frame->payload[index]);
        }

        LMud_GC_MarkObject(self, frame->function);
        LMud_GC_MarkFrame(self, LMud_Frame_GetLexical(frame));
        frame = frame->previous;
    }
}

static bool LMud_GC_PopPending(struct LMud_GC* self, struct LMud_Header** header)
{
    struct LMud_Header*  next;

    if (self->pending == NULL)
        return false;
    else {
        next          = self->pending;
        self->pending = LMud_Header_GetLink(self->pending);
        LMud_Header_SetLink(next, NULL);
        *header       = next;
        return true;
    }
}

static void LMud_GC_Loop(struct LMud_GC* self)
{
    struct LMud_Header*  header;
    void*                object;

    while (LMud_GC_PopPending(self, &header))
    {
        object = LMud_Header_ToObject(header);
        LMud_Header_SetGCBits(header, LMud_GCBits_Black);
        header->type->marker(self, object);
    }
}

static void LMud_GC_Collect(struct LMud_GC* self)
{
    struct LMud_Header**  iterator;
    struct LMud_Header**  next;
    struct LMud_Header*   header;

    iterator = &self->lisp->objects.objects;
    next     = iterator;

    while (*iterator != NULL)
    {
        header = *iterator;

        switch (LMud_Header_GetGCBits(header))
        {
            case LMud_GCBits_White:
                *iterator = header->next;
                header->type->destructor(LMud_Header_ToObject(header));
                LMud_Free(header);
                self->stats.objects_freed++;
                break;

            case LMud_GCBits_Black:
                LMud_Header_SetGCBits(header, LMud_GCBits_White);
                next = &header->next;
                self->stats.objects_kept++;
                break;
            
            case LMud_GCBits_Grey:
            default:
                assert(false);  // Should not happen
                break;
        }

        iterator = next;
    }
}

static void LMud_GC_MarkRoots(struct LMud_GC* self)
{
    LMud_Mark(self, self->lisp->mud);
}

static void LMud_GC_InitialBookeeping(struct LMud_GC* self)
{
    (void) self;
}

static void LMud_GC_FinalBookeeping(struct LMud_GC* self)
{
    LMud_Objects_ClearGcAllocationCounter(&self->lisp->objects);
    LMud_TrimMalloc();
}

void LMud_GC_Run(struct LMud_GC* self)
{
    LMud_GC_InitialBookeeping(self);
    LMud_GC_MarkRoots(self);
    LMud_GC_Loop(self);
    LMud_GC_Collect(self);
    LMud_GC_FinalBookeeping(self);
}
