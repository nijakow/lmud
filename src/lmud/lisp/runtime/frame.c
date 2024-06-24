
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


LMud_Any LMud_Frame_GetRegister(struct LMud_Frame* self, LMud_Size index)
{
    return self->payload[index];
}

void LMud_Frame_SetRegister(struct LMud_Frame* self, LMud_Size index, LMud_Any value)
{
    self->payload[index] = value;
}


void LMud_Frame_Push(struct LMud_Frame* self, LMud_Any value)
{
    self->payload[self->sp++] = value;
}

LMud_Any LMud_Frame_Pop(struct LMud_Frame* self)
{
    return self->payload[--self->sp];
}

void LMud_Frame_Drop(struct LMud_Frame* self, LMud_Size count)
{
    self->sp -= count;
}
