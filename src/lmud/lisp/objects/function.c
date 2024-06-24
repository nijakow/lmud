
#include "function.h"

void LMud_Function_Create(struct LMud_Function* self, struct LMud_ArgInfo info, LMud_Any bytecodes, LMud_Any constants)
{
    self->info      = info;
    self->bytecodes = bytecodes;
    self->constants = constants;
}

void LMud_Function_Destroy(struct LMud_Function* self)
{
    (void) self;
}


LMud_Any LMud_Function_Bytecodes(struct LMud_Function* self)
{
    return self->bytecodes;
}

LMud_Any LMud_Function_Constants(struct LMud_Function* self)
{
    return self->constants;
}


bool LMud_Function_IsLexicalized(struct LMud_Function* self)
{
    return self->info.lexicalized;
}
