
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/lisp/runtime/fiber.h>

struct LMud_Process
{
    struct LMud_Fiber*  fiber;
};

void LMud_Process_Create(struct LMud_Process* self, struct LMud_Fiber* fiber);
void LMud_Process_Destroy(struct LMud_Process* self);
void LMud_Process_Mark(struct LMud_GC* gc, struct LMud_Process* self);
LMud_Size LMud_Process_CalculateSizeInBytes(struct LMud_Process* self);

struct LMud_Fiber* LMud_Process_GetFiber(struct LMud_Process* self);

LMud_Any LMud_Process_GetStateAsLispValue(struct LMud_Process* self, struct LMud_Lisp* lisp);

void LMud_Process_FiberWait(struct LMud_Process* self, struct LMud_Fiber* fiber);
