
#include <lmud/lisp/gc.h>
#include <lmud/util/memory.h>

#include "builtin.h"

void LMud_Builtin_Create(struct LMud_Builtin* self, const char* name, LMud_BuiltinFunction function)
{
    self->name     = LMud_Strdup(name);
    self->function = function;
}

void LMud_Builtin_Destroy(struct LMud_Builtin* self)
{
    LMud_Free(self->name);
}

void LMud_Builtin_Mark(struct LMud_GC* gc, struct LMud_Builtin* self)
{
    (void) gc;
    (void) self;
}

LMud_Size LMud_Builtin_CalculateSizeInBytes(struct LMud_Builtin* self)
{
    return sizeof(struct LMud_Builtin) + LMud_CStr_Length(self->name) + 1;
}
