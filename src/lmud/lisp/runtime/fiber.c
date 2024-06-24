
#include <lmud/util/memory.h>

#include "fiber.h"


void LMud_Fiber_Create(struct LMud_Fiber* self)
{
    self->stack         =  LMud_Alloc(1024 * 1024);
    self->stack_roof    = &self->stack[1024 * 1024];
    self->stack_pointer =  self->stack;
}

void LMud_Fiber_Destroy(struct LMud_Fiber* self)
{
    (void) self;
}

static void* LMud_Fiber_StackTop(struct LMud_Fiber* self)
{
    return self->stack_pointer;
}

struct LMud_Frame* LMud_Fiber_PushFrame(struct LMud_Fiber* self, struct LMud_Function* function, struct LMud_Frame* lexical, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_Frame*  frame;
    
    frame               = LMud_Fiber_StackTop(self);
    self->stack_pointer = self->stack_pointer + sizeof(struct LMud_Frame) + (function->args.register_count + function->args.stack_size) * sizeof(LMud_Any);

    LMud_Frame_Create(frame, self->top, lexical, function, arguments, argument_count);

    return frame;
}

void LMud_Fiber_PopFrame(struct LMud_Fiber* self)
{
    struct LMud_Frame*  frame;
    
    /*
     * TODO: Handle 'floating' frames.
     */

    frame               = self->top;
    self->top           = frame->previous;
    self->stack_pointer = (char*) frame;
}
