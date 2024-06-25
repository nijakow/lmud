
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/lisp/objects/function.h>


struct LMud_FrameRef
{
    struct LMud_Frame*     frame;
    struct LMud_FrameRef*  next;
};

void LMud_FrameRef_Create(struct LMud_FrameRef* self, struct LMud_Frame* frame);
void LMud_FrameRef_Destroy(struct LMud_FrameRef* self);

struct LMud_Frame* LMud_FrameRef_GetFrame(struct LMud_FrameRef* self);


struct LMud_Frame
{
    struct LMud_FrameRef*  references;
    struct LMud_Frame*     previous;
    struct LMud_Frame*     child;
    struct LMud_FrameRef   lexical;
    struct LMud_Function*  function;
    uint16_t               ip;
    uint8_t                sp;
    uint8_t                ap;
    uint8_t                ac;
    LMud_Any               payload[];
};

void LMud_Frame_Create(struct LMud_Frame*    self,
                       struct LMud_Frame*    previous,
                       struct LMud_Frame*    lexical,
                       struct LMud_Function* function,
                       LMud_Any*             arguments,
                       LMud_Size             extra_argument_count);

void LMud_Frame_Destroy(struct LMud_Frame* self);

void LMud_Frame_RemoveReference(struct LMud_Frame* self, struct LMud_FrameRef* reference);

LMud_Size LMud_Frame_RemainingExtraArgumentCount(struct LMud_Frame* self);
bool      LMud_Frame_HasExtraArguments(struct LMud_Frame* self);
bool      LMud_Frame_TakeExtraArgument(struct LMud_Frame* self, LMud_Any* value);

LMud_Any LMud_Frame_GetRegister(struct LMud_Frame* self, LMud_Size index);
void     LMud_Frame_SetRegister(struct LMud_Frame* self, LMud_Size index, LMud_Any value);

void      LMud_Frame_Push(struct LMud_Frame* self, LMud_Any value);
LMud_Any  LMud_Frame_Pop(struct LMud_Frame* self);
LMud_Any* LMud_Frame_PopN(struct LMud_Frame* self, LMud_Size count);
void      LMud_Frame_Drop(struct LMud_Frame* self, LMud_Size count);
