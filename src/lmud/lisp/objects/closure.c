
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


struct LMud_Function* LMud_Closure_GetFunction(struct LMud_Closure* self)
{
    return self->function;
}

struct LMud_Frame* LMud_Closure_GetLexical(struct LMud_Closure* self)
{
    return LMud_FrameRef_GetFrame(&self->lexical);
}
