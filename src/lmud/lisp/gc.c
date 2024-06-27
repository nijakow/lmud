
#include <lmud/lisp/lisp.h>
#include <lmud/lisp/objects.h>
#include <lmud/util/memory.h>

#include "gc.h"


void LMud_GC_Create(struct LMud_GC* self, struct LMud_Lisp* lisp)
{
    self->pending = NULL;
    self->lisp    = lisp;
}

void LMud_GC_Destroy(struct LMud_GC* self)
{
    assert(self->pending == NULL);
}

void LMud_GC_MarkAny(struct LMud_GC* self, struct LMud_Any any)
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

    if (header->bits.gc == LMud_GCBits_White)
    {
        header->bits.gc = LMud_GCBits_Grey;
        header->link    = self->pending;
        self->pending   = header;
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
        LMud_GC_MarkFrame(self, LMud_FrameRef_GetFrame(&frame->lexical));
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
        self->pending = self->pending->link;
        next->link    = NULL;
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
        object          = LMud_Header_ToObject(header);
        header->bits.gc = LMud_GCBits_Black;
        header->type->marker(self, object);
    }
}

static void LMud_GC_Collect(struct LMud_GC* self)
{
    struct LMud_Header**  iterator;
    struct LMud_Header**  next;
    struct LMud_Header*   header;

    for (iterator = &self->lisp->objects.objects; *iterator != NULL; iterator = next)
    {
        header = *iterator;

        switch (header->bits.gc)
        {
            case LMud_GCBits_White:
                *iterator = header->next;
                header->type->destructor(LMud_Header_ToObject(header));
                LMud_Free(header);
                break;

            case LMud_GCBits_Black:
                next = &header->next;
                break;
            
            case LMud_GCBits_Grey:
            default:
                assert(false);  // Should not happen
                break;
        }
    }
}

static void LMud_GC_MarkRoots(struct LMud_GC* self)
{
    LMud_Lisp_Mark(self, self->lisp);
}

void LMud_GC_Run(struct LMud_GC* self)
{
    LMud_GC_MarkRoots(self);
    LMud_GC_Loop(self);
    LMud_GC_Collect(self);
}
