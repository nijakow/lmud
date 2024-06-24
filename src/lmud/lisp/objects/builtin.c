
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
