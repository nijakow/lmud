
#include "function.h"


void LMud_Function_Create(struct LMud_Function* self, struct LMud_ArgInfo args, LMud_Any bytecodes, LMud_Any constants)
{
    self->args      = args;
    self->bytecodes = bytecodes;
    self->constants = constants;
}

void LMud_Function_Destroy(struct LMud_Function* self)
{
    (void) self;
}
