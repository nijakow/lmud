
#include "frame.h"

void LMud_Frame_Create(struct LMud_Frame*    self,
                       struct LMud_Frame*    previous,
                       struct LMud_Frame*    lexical,
                       struct LMud_Function* function,
                       LMud_Any*             arguments_base,
                       LMud_Size             arguments_count)
{
    self->previous       = previous;
    self->lexical        = lexical;
    self->arguments_base = arguments_base;
    self->arguments_top  = arguments_base + arguments_count;
    self->function       = function;
    self->ip             = 0;
    self->sp             = function->args.register_count;
}

void LMud_Frame_Destroy(struct LMud_Frame* self)
{
    (void) self;
}
