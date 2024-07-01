
#include <lmud/lisp/gc.h>

#include "function.h"

void LMud_Function_Create(struct LMud_Function* self, struct LMud_ArgInfo info, LMud_Any bytecodes, LMud_Any constants, LMud_Any source_code)
{
    self->info        = info;
    self->bytecodes   = bytecodes;
    self->constants   = constants;
    self->source_code = source_code;
}

void LMud_Function_Destroy(struct LMud_Function* self)
{
    (void) self;
}

void LMud_Function_Mark(struct LMud_GC* gc, struct LMud_Function* self)
{
    LMud_GC_MarkAny(gc, self->bytecodes);
    LMud_GC_MarkAny(gc, self->constants);
    LMud_GC_MarkAny(gc, self->source_code);
}

LMud_Size LMud_Function_CalculateSizeInBytes(struct LMud_Function* self)
{
    (void) self;
    return sizeof(struct LMud_Function);
}


LMud_Any LMud_Function_Bytecodes(struct LMud_Function* self)
{
    return self->bytecodes;
}

LMud_Any LMud_Function_Constants(struct LMud_Function* self)
{
    return self->constants;
}

LMud_Any LMud_Function_SourceCode(struct LMud_Function* self)
{
    return self->source_code;
}


bool LMud_Function_IsLexicalized(struct LMud_Function* self)
{
    return self->info.lexicalized;
}
