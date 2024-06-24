
#pragma once

#include <lmud/lisp/base.h>

struct LMud_Interpreter
{
    struct LMud_Fiber*  fiber;
};

void LMud_Interpreter_Create(struct LMud_Interpreter* self, struct LMud_Fiber* fiber);
void LMud_Interpreter_Destroy(struct LMud_Interpreter* self);

void LMud_Interpreter_Tick(struct LMud_Interpreter* self);
