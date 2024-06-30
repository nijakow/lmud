
#pragma once

#include <lmud/lisp/base.h>

typedef void (*LMud_BuiltinFunction)(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count);

struct LMud_Builtin
{
    char*                 name;
    LMud_BuiltinFunction  function;
};

void LMud_Builtin_Create(struct LMud_Builtin* self, const char* name, LMud_BuiltinFunction function);
void LMud_Builtin_Destroy(struct LMud_Builtin* self);
void LMud_Builtin_Mark(struct LMud_GC* gc, struct LMud_Builtin* self);
LMud_Size LMud_Builtin_CalculateSizeInBytes(struct LMud_Builtin* self);

const char* LMud_Builtin_GetName(struct LMud_Builtin* self);
