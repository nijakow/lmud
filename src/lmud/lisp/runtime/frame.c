
#include "frame.h"


void LMud_FrameRef_Create(struct LMud_FrameRef* self, struct LMud_Frame* frame)
{
    self->frame = frame;
    
    if (frame == NULL) {
        self->next = NULL;
    } else {
        self->next        = frame->references;
        frame->references = self;
    }
}

void LMud_FrameRef_Destroy(struct LMud_FrameRef* self)
{
    if (self->frame != NULL)
    {
        LMud_Frame_RemoveReference(self->frame, self);
    }
}

struct LMud_Frame* LMud_FrameRef_GetFrame(struct LMud_FrameRef* self)
{
    return self->frame;
}


void LMud_Frame_Create(struct LMud_Frame*    self,
                       struct LMud_Frame*    previous,
                       struct LMud_Frame*    lexical,
                       struct LMud_Function* function,
                       LMud_Any*             arguments,
                       LMud_Size             extra_argument_count)
{
    LMud_Size  index;

    self->previous       = previous;
    self->child          = NULL;
    
    LMud_FrameRef_Create(&self->lexical, lexical);

    self->function       = function;
    self->ip             = 0;
    self->sp             = function->info.register_count;
    self->ap             = function->info.register_count + function->info.stack_size + extra_argument_count;
    self->ac             = function->info.register_count + function->info.stack_size;

    for (index = 0; index < function->info.fixed_argument_count; ++index)
    {
        self->payload[index] = arguments[index];
    }

    for (index = 0; index < extra_argument_count; ++index)
    {
        self->payload[function->info.register_count + function->info.stack_size + index] = arguments[function->info.fixed_argument_count + index];
    }
}

void LMud_Frame_Destroy(struct LMud_Frame* self)
{
    assert(self->references == NULL);
    LMud_FrameRef_Destroy(&self->lexical);
}

void LMud_Frame_RemoveReference(struct LMud_Frame* self, struct LMud_FrameRef* reference)
{
    struct LMud_FrameRef**  current;

    current = &self->references;

    while (*current != NULL)
    {
        if (*current == reference)
        {
            *current = reference->next;
            return;
        }

        current = &(*current)->next;
    }

    /*
     * TODO: If the reference count drops to zero and if we are heap-allocated,
     *       we should free ourselves.
     */
}


LMud_Size LMud_Frame_RemainingExtraArgumentCount(struct LMud_Frame* self)
{
    return self->ap - self->ac;
}

bool LMud_Frame_HasExtraArguments(struct LMud_Frame* self)
{
    return LMud_Frame_RemainingExtraArgumentCount(self) > 0;
}

bool LMud_Frame_GetExtraArgument(struct LMud_Frame* self, LMud_Size index, LMud_Any* value)
{
    if (index < LMud_Frame_RemainingExtraArgumentCount(self)) {
        if (value != NULL)
            *value = self->payload[self->ac + index];
        return true;
    } else {
        return false;
    }
}

bool LMud_Frame_PeekExtraArgument(struct LMud_Frame* self, LMud_Any* value)
{
    if (LMud_Frame_HasExtraArguments(self)) {
        if (value != NULL)
            *value = self->payload[self->ac];
        return true;
    } else {
        return false;
    }
}

bool LMud_Frame_TakeExtraArgument(struct LMud_Frame* self, LMud_Any* value)
{
    if (LMud_Frame_HasExtraArguments(self)) {
        if (value != NULL)
            *value = self->payload[self->ac];
        self->ac++;
        return true;
    } else {
        return false;
    }
}

bool LMud_Frame_PickKeywordArgument(struct LMud_Frame* self, LMud_Any key, LMud_Any* value)
{
    LMud_Size  index;

    for (index = self->ac; index < self->ap; index += 2)
    {
        if (LMud_Any_Eq(self->payload[index], key))
        {
            if (index + 1 >= self->ap)
                return false;
            else {
                if (value != NULL)
                    *value = self->payload[index + 1];
                return true;
            }
        }
    }

    return false;
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

LMud_Any* LMud_Frame_PopN(struct LMud_Frame* self, LMud_Size count)
{
    self->sp -= count;
    return &self->payload[self->sp];
}

void LMud_Frame_Drop(struct LMud_Frame* self, LMud_Size count)
{
    self->sp -= count;
}
