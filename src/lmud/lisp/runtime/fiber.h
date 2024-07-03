
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/lisp/bytecodes.h>
#include <lmud/lisp/runtime/frame.h>

#define LMud_Fiber_MAX_ACCUMULATORS 16

struct LMud_FiberQueue
{
    struct LMud_Fiber*  fibers;
};

void LMud_FiberQueue_Create(struct LMud_FiberQueue* self);
void LMud_FiberQueue_Destroy(struct LMud_FiberQueue* self);

bool LMud_FiberQueue_IsEmpty(struct LMud_FiberQueue* self);
bool LMud_FiberQueue_HasFibers(struct LMud_FiberQueue* self);

void LMud_FiberQueue_AddFiber(struct LMud_FiberQueue* self, struct LMud_Fiber* fiber);


struct LMud_FiberRef
{
    struct LMud_Fiber*      fiber;

    struct LMud_FiberRef**  prev;
    struct LMud_FiberRef*   next;
};

void LMud_FiberRef_Create(struct LMud_FiberRef* self, struct LMud_Fiber* fiber);
void LMud_FiberRef_Destroy(struct LMud_FiberRef* self);

void LMud_FiberRef_Link(struct LMud_FiberRef* self, struct LMud_FiberRef** list);
void LMud_FiberRef_Unlink(struct LMud_FiberRef* self);

struct LMud_Fiber* LMud_FiberRef_Get(struct LMud_FiberRef* self);

void LMud_FiberRef_Set(struct LMud_FiberRef* self, struct LMud_Fiber* fiber);


enum LMud_FiberState
{
    LMud_FiberState_CREATED,
    LMud_FiberState_RUNNING,
    LMud_FiberState_WAITING,
    LMud_FiberState_YIELDING,
    LMud_FiberState_TERMINATED
};


struct LMud_Fiber
{
    struct LMud_Lisp*              lisp;
    struct LMud_Scheduler*         scheduler;

    struct LMud_Fiber**            prev;
    struct LMud_Fiber*             next;

    struct LMud_FiberRef*          references;

    struct LMud_Fiber**            queue_prev;
    struct LMud_Fiber*             queue_next;

    struct LMud_Frame*             top;
    char*                          stack;
    char*                          stack_roof;
    char*                          stack_pointer;

    LMud_Size                      accumulator_count;
    LMud_Any                       accumulator[LMud_Fiber_MAX_ACCUMULATORS];

    LMud_Any                       port;

    struct LMud_FrameList          floating_frames;

    enum LMud_FiberState           state;
    enum LMud_ExecutionResumption  execution_mode;
};

void LMud_Fiber_Create(struct LMud_Fiber* self, struct LMud_Lisp* lisp, struct LMud_Scheduler* scheduler);
void LMud_Fiber_Destroy(struct LMud_Fiber* self);
void LMud_Fiber_Mark(struct LMud_GC* gc, struct LMud_Fiber* self);

void LMud_Fiber_Link(struct LMud_Fiber* self, struct LMud_Fiber** list);
void LMud_Fiber_Unlink(struct LMud_Fiber* self);

void LMud_Fiber_LinkQueue(struct LMud_Fiber* self, struct LMud_Fiber** list);
void LMud_Fiber_UnlinkQueue(struct LMud_Fiber* self);
void LMud_Fiber_MoveToQueue(struct LMud_Fiber* self, struct LMud_FiberQueue* queue);

enum LMud_FiberState LMud_Fiber_GetState(struct LMud_Fiber* self);

bool LMud_Fiber_IsRunning(struct LMud_Fiber* self);
bool LMud_Fiber_IsWaiting(struct LMud_Fiber* self);
bool LMud_Fiber_IsYielding(struct LMud_Fiber* self);
bool LMud_Fiber_HasTerminated(struct LMud_Fiber* self);

void LMud_Fiber_ControlStart(struct LMud_Fiber* self);
void LMud_Fiber_ControlRestartWithValue(struct LMud_Fiber* self, LMud_Any value);
void LMud_Fiber_ControlWaitOnQueue(struct LMud_Fiber* self, struct LMud_FiberQueue* queue);
void LMud_Fiber_ControlYield(struct LMud_Fiber* self);
void LMud_Fiber_ControlUnyield(struct LMud_Fiber* self);
void LMud_Fiber_ControlTerminate(struct LMud_Fiber* self);

enum LMud_ExecutionResumption LMud_Fiber_GetExecutionResumptionMode(struct LMud_Fiber* self);
void                          LMud_Fiber_SetExecutionResumptionMode(struct LMud_Fiber* self, enum LMud_ExecutionResumption mode);

bool LMud_Fiber_HasFrames(struct LMud_Fiber* self);

LMud_Any   LMud_Fiber_GetAccumulator(struct LMud_Fiber* self);
void       LMud_Fiber_SetAccumulator(struct LMud_Fiber* self, LMud_Any value);
void       LMud_Fiber_Values(struct LMud_Fiber* self, LMud_Any* values, LMud_Size count);
LMud_Size  LMud_Fiber_ValueCount(struct LMud_Fiber* self);
LMud_Any   LMud_Fiber_GetValue(struct LMud_Fiber* self, LMud_Size index);

LMud_Any   LMud_Fiber_GetPort(struct LMud_Fiber* self);
void       LMud_Fiber_SetPort(struct LMud_Fiber* self, LMud_Any port);

struct LMud_Frame* LMud_Fiber_PushFrame(struct LMud_Fiber* self, struct LMud_Function* function, struct LMud_Frame* lexical, LMud_Any* arguments, LMud_Size argument_count);
void               LMud_Fiber_PopFrame(struct LMud_Fiber* self);

void LMud_Fiber_Enter(struct LMud_Fiber* self, LMud_Any function, LMud_Any* arguments, LMud_Size argument_count);
void LMud_Fiber_EnterThunk(struct LMud_Fiber* self, LMud_Any function);

void LMud_Fiber_PerformCall(struct LMud_Fiber* self, LMud_Any function, LMud_Size argument_count);
void LMud_Fiber_PerformReturn(struct LMud_Fiber* self);
void LMud_Fiber_PerformError(struct LMud_Fiber* self, const char* message);

void LMud_Fiber_SignalAndUnwind(struct LMud_Fiber* self);
void LMud_Fiber_SignalAndUnwindWithValues(struct LMud_Fiber* self, LMud_Any* values, LMud_Size count);

void LMud_Fiber_Tick(struct LMud_Fiber* self);

void LMud_Fiber_Dump(struct LMud_Fiber* self);
