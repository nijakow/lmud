
#include "closure.h"

void LMud_Closure_Create(struct LMud_Closure* self, struct LMud_Function* function, struct LMud_Frame* lexical)
{
    self->function = function;
    LMud_FrameRef_Create(&self->lexical, lexical);
}

void LMud_Closure_Destroy(struct LMud_Closure* self)
{
    LMud_FrameRef_Destroy(&self->lexical);
}
