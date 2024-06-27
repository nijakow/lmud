
#include <lmud/lisp/gc.h>
#include <lmud/lisp/lisp.h>
#include <lmud/lisp/objects/closure.h>
#include <lmud/lisp/objects/function.h>
#include <lmud/lisp/runtime/interpreter.h>
#include <lmud/util/memory.h>
#include <lmud/util/vt100.h>

#include "fiber.h"


void LMud_Fiber_Create(struct LMud_Fiber* self, struct LMud_Lisp* lisp)
{
    self->lisp          = lisp;

    self->prev          = NULL;
    self->next          = NULL;

    self->top           = NULL;

    self->stack         =  LMud_Alloc(1024 * 1024);
    self->stack_roof    = &self->stack[1024 * 1024];
    self->stack_pointer =  self->stack;

    self->accumulator_count = 1;
    self->accumulator[0]    = LMud_Lisp_Nil(lisp);

    LMud_FrameList_Create(&self->floating_frames);
}

void LMud_Fiber_Destroy(struct LMud_Fiber* self)
{
    LMud_FrameList_Destroy(&self->floating_frames);
    LMud_Free(self->stack);
    LMud_Fiber_Unlink(self);
}

void LMud_Fiber_Mark(struct LMud_GC* gc, struct LMud_Fiber* self)
{
    LMud_Size  index;

    for (index = 0; index < self->accumulator_count; index++)
    {
        LMud_GC_MarkAny(gc, self->accumulator[index]);
    }

    LMud_GC_MarkFrame(gc, self->top);  // This will run recursively.
}


void LMud_Fiber_Link(struct LMud_Fiber* self, struct LMud_Fiber** list)
{
    LMud_Fiber_Unlink(self);

    self->prev =  list;
    self->next = *list;
    if (*list != NULL)
        (*list)->prev = &self->next;
    *list = self;
}

void LMud_Fiber_Unlink(struct LMud_Fiber* self)
{
    if (self->prev != NULL)
        *self->prev = self->next;
    if (self->next != NULL)
        self->next->prev = self->prev;
    self->prev = NULL;
    self->next = NULL;
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

void LMud_Fiber_Values(struct LMud_Fiber* self, LMud_Any* values, LMud_Size count)
{
    LMud_Size  i;

    if (count == 0)
        self->accumulator[0] = LMud_Lisp_Nil(self->lisp);
    else {
        for (i = 0; i < count; i++)
        {
            self->accumulator[i] = values[i];
        }
    }

    self->accumulator_count = count;
}

LMud_Size LMud_Fiber_ValueCount(struct LMud_Fiber* self)
{
    return self->accumulator_count;
}

LMud_Any LMud_Fiber_GetValue(struct LMud_Fiber* self, LMud_Size index)
{
    return self->accumulator[index];
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

    if (argument_count < function->info.fixed_argument_count) {
        LMud_Fiber_PerformError(self, "Not enough arguments!");
        assert(false);
        return NULL;
    } else if (extra_args > 0 && !function->info.variadic) {
        LMud_Fiber_PerformError(self, "Function is not variadic!");
        assert(false);
        return NULL;
    }
    
    frame               = LMud_Fiber_StackTop(self);
    self->stack_pointer = self->stack_pointer + sizeof(struct LMud_Frame) + (function->info.register_count + function->info.stack_size + extra_args) * sizeof(LMud_Any);

    LMud_Frame_Create(frame, self->top, lexical, function, arguments, extra_args);

    if (self->top != NULL) {
        self->top->child = frame;
    }

    self->top = frame;

    return frame;
}

void LMud_Fiber_PopFrame(struct LMud_Fiber* self)
{
    struct LMud_Frame*  frame;
    struct LMud_Frame*  frame2;
    
    /*
     * TODO: Handle 'floating' frames.
     */

    frame               = self->top;
    self->top           = frame->previous;
    self->stack_pointer = (char*) frame;

    if (self->top != NULL) {
        self->top->child = NULL;
    }

    if (LMud_Frame_ShouldBeMovedToShip(frame)) {
        frame2 = &LMud_FrameList_Insert(&self->floating_frames, frame)->frame;
        printf("[Note]: Frame %p is moved to a ship and is now %p.\n", frame, frame2);
    } else {
        LMud_Frame_Destroy(frame);
    }
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


void LMud_Fiber_CallCustomObject(struct LMud_Fiber* self, LMud_Any custom, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Size  index;
    LMud_Any   new_arguments[argument_count + 1];

    assert(LMud_Lisp_IsCustom(self->lisp, custom));

    new_arguments[0] = custom;

    for (index = 0; index < argument_count; index++)
    {
        new_arguments[index + 1] = arguments[index];
    }

    LMud_Fiber_Enter(self, LMud_Lisp_CustomDispatcherFunction(self->lisp), new_arguments, argument_count + 1);
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
    } else if (LMud_Lisp_IsBuiltin(self->lisp, function)) {
        ((struct LMud_Builtin*) LMud_Any_AsPointer(function))->function(self, arguments, argument_count);
    } else if (LMud_Lisp_IsCustom(self->lisp, function)) {
        LMud_Fiber_CallCustomObject(self, function, arguments, argument_count);
    } else {
        LMud_Fiber_PerformError(self, "Not a function.");
    }
}

void LMud_Fiber_EnterThunk(struct LMud_Fiber* self, LMud_Any function)
{
    LMud_Fiber_Enter(self, function, NULL, 0);
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
    printf(LMud_VT100_Italic "; " LMud_VT100_Red LMud_VT100_Blink "Error: %s" LMud_VT100_Normal "\n", message);
    LMud_Fiber_Unwind(self);
}


void LMud_Fiber_Tick(struct LMud_Fiber* self)
{
    struct LMud_Interpreter  interpreter;

    LMud_Interpreter_Create(&interpreter, self);
    LMud_Interpreter_Tick(&interpreter);
    LMud_Interpreter_Destroy(&interpreter);
}
