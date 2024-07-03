
#include <lmud/lisp/gc.h>
#include <lmud/lisp/lisp.h>
#include <lmud/lisp/objects/closure.h>
#include <lmud/lisp/objects/function.h>
#include <lmud/lisp/runtime/interpreter.h>
#include <lmud/util/memory.h>
#include <lmud/util/vt100.h>

#include "fiber.h"


static void LMud_Fiber_Unwind(struct LMud_Fiber* self, enum LMud_ExecutionResumption resumption);


void LMud_FiberQueue_Create(struct LMud_FiberQueue* self)
{
    self->fibers = NULL;
}

void LMud_FiberQueue_Destroy(struct LMud_FiberQueue* self)
{
    /*
     * TODO, FIXME, XXX:
     *
     * In most cases, a FiberQueue acts as a list of waiting fibers that want to be
     * reactivated as soon as a specific condition is met.
     * 
     * Just clearing this list by unlinking the fiber would result in a bunch of zombies
     * that are never going to be reactivated.
     */

    if (self->fibers != NULL)
    {
        LMud_Debugf(self->fibers->lisp->mud, LMud_LogLevel_WARNING, "Fiber queue %p is being destroyed with fibers still in it!", self);
    }

    while (self->fibers != NULL)
    {
        LMud_Fiber_UnlinkQueue(self->fibers);
    }
}

bool LMud_FiberQueue_IsEmpty(struct LMud_FiberQueue* self)
{
    return self->fibers == NULL;
}

bool LMud_FiberQueue_HasFibers(struct LMud_FiberQueue* self)
{
    return self->fibers != NULL;
}

void LMud_FiberQueue_AddFiber(struct LMud_FiberQueue* self, struct LMud_Fiber* fiber)
{
    LMud_Fiber_MoveToQueue(fiber, self);
}

void LMud_FiberQueue_WakeUpAllWithValues(struct LMud_FiberQueue* self, LMud_Any* values, LMud_Size count)
{
    struct LMud_Fiber*  fiber;

    while (self->fibers != NULL)
    {
        fiber = self->fibers;
        LMud_Fiber_Values(fiber, values, count);
        LMud_Fiber_ControlStart(fiber);
    }
}

void LMud_FiberQueue_WakeUpAllWithValue(struct LMud_FiberQueue* self, LMud_Any value)
{
    LMud_FiberQueue_WakeUpAllWithValues(self, &value, 1);
}


void LMud_FiberRef_Create(struct LMud_FiberRef* self, struct LMud_Fiber* fiber)
{
    self->fiber = fiber;
    self->prev  = NULL;
    self->next  = NULL;

    if (fiber != NULL)
    {
        LMud_FiberRef_Link(self, &fiber->references);
    }
}

void LMud_FiberRef_Destroy(struct LMud_FiberRef* self)
{
    if (self->fiber != NULL)
    {
        LMud_FiberRef_Unlink(self);
    }
}

void LMud_FiberRef_Mark(struct LMud_GC* gc, struct LMud_FiberRef* self)
{
    // Do nothing, the fibers will be marked automatically
    (void) gc;
    (void) self;
}


void LMud_FiberRef_Link(struct LMud_FiberRef* self, struct LMud_FiberRef** list)
{
    LMud_FiberRef_Unlink(self);

    self->prev =  list;
    self->next = *list;
    if (*list != NULL)
        (*list)->prev = &self->next;
    *list = self;
}

void LMud_FiberRef_Unlink(struct LMud_FiberRef* self)
{
    if (self->prev != NULL)
        *self->prev = self->next;
    if (self->next != NULL)
        self->next->prev = self->prev;
    self->prev  = NULL;
    self->next  = NULL;
    self->fiber = NULL;
}

struct LMud_Fiber* LMud_FiberRef_Get(struct LMud_FiberRef* self)
{
    return self->fiber;
}

void LMud_FiberRef_Set(struct LMud_FiberRef* self, struct LMud_Fiber* fiber)
{
    if (self->fiber != fiber)
    {
        if (self->fiber != NULL)
        {
            LMud_FiberRef_Unlink(self);
        }

        self->fiber = fiber;

        if (fiber != NULL)
        {
            LMud_FiberRef_Link(self, &fiber->references);
        }
    }
}



void LMud_Fiber_Create(struct LMud_Fiber* self, struct LMud_Lisp* lisp, struct LMud_Scheduler* scheduler)
{
    self->lisp          = lisp;
    self->scheduler     = scheduler;

    self->prev          = NULL;
    self->next          = NULL;

    self->references    = NULL;

    self->queue_prev    = NULL;
    self->queue_next    = NULL;

    self->top           = NULL;

    self->stack         =  LMud_Alloc(1024 * 1024);
    self->stack_roof    = &self->stack[1024 * 1024];
    self->stack_pointer =  self->stack;

    self->accumulator_count = 1;
    self->accumulator[0]    = LMud_Lisp_Nil(lisp);

    self->self_process  = LMud_Objects_Process(&lisp->objects, self);

    self->port          = LMud_Lisp_Nil(lisp);

    LMud_FrameList_Create(&self->floating_frames);

    LMud_FiberQueue_Create(&self->waiting_for_result);

    self->state          = LMud_FiberState_CREATED;
    self->execution_mode = LMud_ExecutionResumption_NORMAL;
}

void LMud_Fiber_Destroy(struct LMud_Fiber* self)
{
    LMud_Debugf(self->lisp->mud, LMud_LogLevel_HALF_DEBUG, "Destroying fiber %p");
    
    // TODO, FIXME, XXX: Remove all references to this fiber.
    
    if (self->references != NULL)
    {
        LMud_Debugf(self->lisp->mud, LMud_LogLevel_FATAL, "Fiber %p is being destroyed with references still pointing to it! System might be unstable due to dangling pointers!", self);
    }

    LMud_Fiber_UnlinkQueue(self);
    LMud_FiberQueue_Destroy(&self->waiting_for_result);
    LMud_FrameList_Destroy(&self->floating_frames);
    LMud_Free(self->stack);
    LMud_Fiber_Unlink(self);
}

void LMud_Fiber_Mark(struct LMud_GC* gc, struct LMud_Fiber* self)
{
    LMud_Size  index;

    LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Marking fiber %p...", self);

    for (index = 0; index < self->accumulator_count; index++)
    {
        LMud_GC_MarkAny(gc, self->accumulator[index]);
    }

    LMud_GC_MarkObject(gc, self->self_process);

    LMud_GC_MarkAny(gc, self->port);

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

void LMud_Fiber_LinkQueue(struct LMud_Fiber* self, struct LMud_Fiber** list)
{
    LMud_Fiber_UnlinkQueue(self);

    self->queue_prev =  list;
    self->queue_next = *list;
    if (*list != NULL)
        (*list)->queue_prev = &self->queue_next;
    *list = self;
}

void LMud_Fiber_UnlinkQueue(struct LMud_Fiber* self)
{
    LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Unlinking fiber %p from queue...", self);

    if (self->queue_prev != NULL)
        *self->queue_prev = self->queue_next;
    if (self->queue_next != NULL)
        self->queue_next->queue_prev = self->queue_prev;
    self->queue_prev = NULL;
    self->queue_next = NULL;
}

void LMud_Fiber_MoveToQueue(struct LMud_Fiber* self, struct LMud_FiberQueue* queue)
{
    LMud_Fiber_LinkQueue(self, &queue->fibers);
}


enum LMud_FiberState LMud_Fiber_GetState(struct LMud_Fiber* self)
{
    return self->state;
}

static void LMud_Fiber_SetState(struct LMud_Fiber* self, enum LMud_FiberState state)
{
    LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Setting state of fiber %p to %d", self, state);
    self->state = state;
}

struct LMud_Process* LMud_Fiber_GetProcess(struct LMud_Fiber* self)
{
    return self->self_process;
}

bool LMud_Fiber_IsRunning(struct LMud_Fiber* self)
{
    return LMud_Fiber_GetState(self) == LMud_FiberState_RUNNING;
}

bool LMud_Fiber_IsWaiting(struct LMud_Fiber* self)
{
    return LMud_Fiber_GetState(self) == LMud_FiberState_WAITING;
}

bool LMud_Fiber_IsYielding(struct LMud_Fiber* self)
{
    return LMud_Fiber_GetState(self) == LMud_FiberState_YIELDING;
}

bool LMud_Fiber_HasTerminated(struct LMud_Fiber* self)
{
    return LMud_Fiber_GetState(self) == LMud_FiberState_TERMINATED;
}

void LMud_Fiber_ControlStart(struct LMud_Fiber* self)
{
    if (!LMud_Fiber_IsRunning(self))
    {
        LMud_Fiber_SetState(self, LMud_FiberState_RUNNING);
        LMud_Scheduler_MoveToRunningQueue(self->scheduler, self);
    }
}

void LMud_Fiber_ControlRestartWithValue(struct LMud_Fiber* self, LMud_Any value)
{
    LMud_Fiber_SetAccumulator(self, value);
    LMud_Fiber_ControlStart(self);
}

void LMud_Fiber_ControlWaitOnQueue(struct LMud_Fiber* self, struct LMud_FiberQueue* queue)
{
    LMud_Fiber_MoveToQueue(self, queue);
    LMud_Fiber_SetState(self, LMud_FiberState_WAITING);
}

void LMud_Fiber_ControlYield(struct LMud_Fiber* self)
{
    LMud_Fiber_SetState(self, LMud_FiberState_YIELDING);
}

void LMud_Fiber_ControlUnyield(struct LMud_Fiber* self)
{
    LMud_Fiber_SetState(self, LMud_FiberState_RUNNING);
}

void LMud_Fiber_ContinueTerminate_FRIEND(struct LMud_Fiber* self)
{
    /*
     * Make sure that we are running, so that the scheduler
     * can properly handle the termination.
     */
    LMud_Fiber_ControlStart(self);
    LMud_Fiber_Unwind(self, LMud_ExecutionResumption_TERMINATING);
}

void LMud_Fiber_FinalizeTerminate_FRIEND(struct LMud_Fiber* self)
{
    LMud_Fiber_SetState(self, LMud_FiberState_TERMINATED);
    LMud_Fiber_UnlinkQueue(self);
    /*
     * Wake up all waiting fibers with our accumulator value(s).
     */
    LMud_FiberQueue_WakeUpAllWithValues(&self->waiting_for_result, self->accumulator, self->accumulator_count);
}

void LMud_Fiber_ControlTerminate(struct LMud_Fiber* self)
{
    LMud_Debugf(self->lisp->mud, LMud_LogLevel_HALF_DEBUG, "Terminating fiber %p ...", self);

    LMud_Fiber_UnlinkQueue(self);
    LMud_Fiber_ContinueTerminate_FRIEND(self);
}

enum LMud_ExecutionResumption LMud_Fiber_GetExecutionResumptionMode(struct LMud_Fiber* self)
{
    return self->execution_mode;
}

void LMud_Fiber_SetExecutionResumptionMode(struct LMud_Fiber* self, enum LMud_ExecutionResumption mode)
{
    self->execution_mode = mode;
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
    if (index >= LMud_Fiber_ValueCount(self))
        return LMud_Lisp_Nil(self->lisp);
    return self->accumulator[index];
}

LMud_Any* LMud_Fiber_RawValues_UNSAFE(struct LMud_Fiber* self)
{
    return self->accumulator;
}


LMud_Any LMud_Fiber_GetPort(struct LMud_Fiber* self)
{
    return self->port;
}

void LMud_Fiber_SetPort(struct LMud_Fiber* self, LMud_Any port)
{
    self->port = port;
}


static void* LMud_Fiber_StackTop(struct LMud_Fiber* self)
{
    return self->stack_pointer;
}

static inline bool LMud_Fiber_HasSpaceOnStack(struct LMud_Fiber* self, LMud_Size size)
{
    return self->stack_pointer + size < self->stack_roof;
}

struct LMud_Frame* LMud_Fiber_PushFrame(struct LMud_Fiber* self, struct LMud_Function* function, struct LMud_Frame* lexical, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_Frame*  frame;
    LMud_Size           extra_args;
    LMud_Size           frame_size;

    extra_args = argument_count - function->info.fixed_argument_count;
    frame_size = sizeof(struct LMud_Frame) + (function->info.register_count + function->info.stack_size + extra_args) * sizeof(LMud_Any);

    if (!LMud_Fiber_HasSpaceOnStack(self, frame_size)) {
        LMud_Fiber_PerformError(self, "Stack overflow!");
        return NULL;
    } else if (argument_count < function->info.fixed_argument_count) {
        LMud_Fiber_PerformError(self, "Not enough arguments!");
        return NULL;
    } else if (extra_args > 0 && !function->info.variadic) {
        LMud_Fiber_PerformError(self, "Too many arguments! (Function is not variadic)");
        return NULL;
    }

    frame               = LMud_Fiber_StackTop(self);
    self->stack_pointer = self->stack_pointer + frame_size;

    LMud_Debugf(
        self->lisp->mud,
        LMud_LogLevel_ALL,
        "Pushing stack frame of size %zu at %p (%zu args / %zu registers + %zu stack slots + %zu extra args)",
        frame_size,
        frame,
        argument_count,
        function->info.fixed_argument_count,
        function->info.stack_size,
        extra_args
    );
    
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
    
    /*
     * TODO: Handle 'floating' frames.
     */

    frame               = self->top;
    self->top           = frame->previous;
    self->stack_pointer = (char*) frame;

    LMud_Debugf(
        self->lisp->mud,
        LMud_LogLevel_ALL,
        "Popping stack frame %p...",
        frame
    );

    if (self->top != NULL) {
        self->top->child = NULL;
    }

    if (LMud_Frame_ShouldBeMovedToShip(frame)) {
        LMud_Debugf(
            self->lisp->mud,
            LMud_LogLevel_ALL,
            "Moving frame %p to floating frames list...",
            frame
        );
        LMud_FrameList_Insert(&self->floating_frames, frame);
    } else {
        LMud_Frame_Destroy(frame);
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
    LMud_Fiber_SetExecutionResumptionMode(self, LMud_ExecutionResumption_NORMAL);
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

static void LMud_Fiber_Unwind(struct LMud_Fiber* self, enum LMud_ExecutionResumption resumption)
{
    LMud_Fiber_SetExecutionResumptionMode(self, resumption);

    LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Unwinding fiber %p to resumption %d ...", self, resumption);

    while (self->top != NULL)
    {
        LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Now unwinding frame %p ...", self->top);

        if (self->top->unwind_protect != LMud_UNWIND_PROTECT_UNDEFINED)
        {
            LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Found an UNWIND-PROTECT block at frame %p", self->top);
            LMud_Frame_SetInstructionPointer(self->top, self->top->unwind_protect);
            break;
        }

        LMud_Fiber_PopFrame(self);
    }
}

void LMud_Fiber_SignalAndUnwind(struct LMud_Fiber* self)
{
    LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Signaling and unwinding fiber %p ...", self);
    LMud_Fiber_Unwind(self, LMud_ExecutionResumption_SIGNAL);
}

void LMud_Fiber_SignalAndUnwindWithValues(struct LMud_Fiber* self, LMud_Any* values, LMud_Size count)
{
    LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Signaling and unwinding fiber %p with %zu values...", self, count);
    LMud_Fiber_Values(self, values, count);
    LMud_Fiber_SignalAndUnwind(self);
}

void LMud_Fiber_PerformError(struct LMud_Fiber* self, const char* message)
{
    LMud_Any  exception;

    LMud_Logf(self->lisp->mud, LMud_LogLevel_ERROR, "Encountered an error on fiber %p: %s\n", self, message);

    exception = LMud_Lisp_String(self->lisp, message);

    LMud_Fiber_SignalAndUnwindWithValues(self, &exception, 1);
}


void LMud_Fiber_Tick(struct LMud_Fiber* self)
{
    struct LMud_Interpreter  interpreter;

    LMud_Interpreter_Create(&interpreter, self);
    LMud_Interpreter_Tick(&interpreter);
    LMud_Interpreter_Destroy(&interpreter);
}


void LMud_Fiber_AddWaitingForResult(struct LMud_Fiber* self, struct LMud_Fiber* fiber)
{
    LMud_Debugf(self->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Fiber %p is waiting for result from fiber %p", self, fiber);
    LMud_Fiber_ControlWaitOnQueue(fiber, &self->waiting_for_result);
}


void LMud_Fiber_Dump(struct LMud_Fiber* self)
{
    struct LMud_Frame*  frame;

    printf("Fiber %p\n", self);
    printf("  State: %d\n", self->state);
    printf("\n");

    printf("Frames:\n");

    frame = self->top;
    while (frame != NULL)
    {
        LMud_Frame_Dump(frame, self->lisp);
        frame = frame->previous;
    }
}
