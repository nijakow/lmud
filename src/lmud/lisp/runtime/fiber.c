
#include <lmud/lisp/lisp.h>
#include <lmud/lisp/objects/closure.h>
#include <lmud/lisp/objects/function.h>
#include <lmud/lisp/runtime/interpreter.h>
#include <lmud/util/memory.h>

#include "fiber.h"


void LMud_Fiber_Create(struct LMud_Fiber* self, struct LMud_Lisp* lisp)
{
    self->lisp          = lisp;

    self->stack         =  LMud_Alloc(1024 * 1024);
    self->stack_roof    = &self->stack[1024 * 1024];
    self->stack_pointer =  self->stack;

    self->accumulator_count = 1;
    self->accumulator[0]    = LMud_Lisp_Nil(lisp);
}

void LMud_Fiber_Destroy(struct LMud_Fiber* self)
{
    (void) self;
}

bool LMud_Fiber_HasFrames(struct LMud_Fiber* self)
{
    return self->top != NULL;
}

LMud_Any LMud_Fiber_GetAccumulator(struct LMud_Fiber* self)
{
    return self->accumulator[0];
}

void LMud_Fiber_SetAccumulator(struct LMud_Fiber* self, LMud_Any value)
{
    self->accumulator[0]    = value;
    self->accumulator_count = 1;
}


static void* LMud_Fiber_StackTop(struct LMud_Fiber* self)
{
    return self->stack_pointer;
}

struct LMud_Frame* LMud_Fiber_PushFrame(struct LMud_Fiber* self, struct LMud_Function* function, struct LMud_Frame* lexical, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_Frame*  frame;
    LMud_Size           extra_args;

    extra_args = argument_count - function->info.fixed_argument_count;

    // TODO: Handle variadic functions.
    
    frame               = LMud_Fiber_StackTop(self);
    self->stack_pointer = self->stack_pointer + sizeof(struct LMud_Frame) + (function->info.register_count + function->info.stack_size + extra_args) * sizeof(LMud_Any);

    LMud_Frame_Create(frame, self->top, lexical, function, arguments, extra_args);

    self->top->child = frame;
    self->top        = frame;

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
    self->top->child    = NULL;
}


void LMud_Fiber_Unwind(struct LMud_Fiber* self)
{
    /*
     * TODO: Implement condition handling.
     */
    while (LMud_Fiber_HasFrames(self))
    {
        LMud_Fiber_PopFrame(self);
    }
}


void LMud_Fiber_Enter(struct LMud_Fiber* self, LMud_Any function, LMud_Any* arguments, LMud_Size argument_count)
{
    if (LMud_Lisp_IsFunction(self->lisp, function)) {
        LMud_Fiber_PushFrame(
            self,
            (struct LMud_Function*) LMud_Any_AsPointer(function),
            NULL,
            arguments,
            argument_count
        );
    } else if (LMud_Lisp_IsClosure(self->lisp, function)) {
        LMud_Fiber_PushFrame(
            self,
            LMud_Closure_GetFunction(LMud_Any_AsPointer(function)),
            LMud_Closure_GetLexical(LMud_Any_AsPointer(function)),
            arguments,
            argument_count
        );
    } else {
        LMud_Fiber_PerformError(self, "Not a function.");
    }
}

void LMud_Fiber_PerformCall(struct LMud_Fiber* self, LMud_Any function, LMud_Size argument_count)
{
    LMud_Fiber_Enter(self, function, LMud_Frame_PopN(self->top, argument_count), argument_count);
}

void LMud_Fiber_PerformReturn(struct LMud_Fiber* self)
{
    LMud_Fiber_PopFrame(self);
}

void LMud_Fiber_PerformError(struct LMud_Fiber* self, const char* message)
{
    printf("  Error: %s\n", message);
    LMud_Fiber_Unwind(self);
}


void LMud_Fiber_Tick(struct LMud_Fiber* self)
{
    struct LMud_Interpreter  interpreter;

    LMud_Interpreter_Create(&interpreter, self);
    LMud_Interpreter_Tick(&interpreter);
    LMud_Interpreter_Destroy(&interpreter);
}
