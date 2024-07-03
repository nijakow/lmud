
#include "process.h"


void LMud_Process_Create(struct LMud_Process* self, struct LMud_Fiber* fiber)
{
    self->fiber = fiber;
}

void LMud_Process_Destroy(struct LMud_Process* self)
{
    (void) self;
}

void LMud_Process_Mark(struct LMud_GC* gc, struct LMud_Process* self)
{
    // Nothing to do, fibers are marked by the scheduler
    (void) gc;
    (void) self;
}

LMud_Size LMud_Process_CalculateSizeInBytes(struct LMud_Process* self)
{
    (void) self;
    return sizeof(struct LMud_Process);
}
