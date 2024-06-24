
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/lisp/objects/function.h>

struct LMud_FrameRef
{
    struct LMud_Frame*  frame;
};

void LMud_FrameRef_Create(struct LMud_FrameRef* self, struct LMud_Frame* frame);
void LMud_FrameRef_Destroy(struct LMud_FrameRef* self);

struct LMud_Frame* LMud_FrameRef_GetFrame(struct LMud_FrameRef* self);


struct LMud_Frame
{
    struct LMud_Frame*     previous;
    struct LMud_FrameRef   lexical;
    LMud_Any*              arguments_base;
    LMud_Any*              arguments_top;
    struct LMud_Function*  function;
    LMud_Size              ip;
    LMud_Size              sp;
    LMud_Any               payload[];
};

void LMud_Frame_Create(struct LMud_Frame*    self,
                       struct LMud_Frame*    previous,
                       struct LMud_Frame*    lexical,
                       struct LMud_Function* function,
                       LMud_Any*             arguments_base,
                       LMud_Size             arguments_count);

void LMud_Frame_Destroy(struct LMud_Frame* self);

LMud_Any LMud_Frame_GetRegister(struct LMud_Frame* self, LMud_Size index);
void     LMud_Frame_SetRegister(struct LMud_Frame* self, LMud_Size index, LMud_Any value);

void     LMud_Frame_Push(struct LMud_Frame* self, LMud_Any value);
LMud_Any LMud_Frame_Pop(struct LMud_Frame* self);
void     LMud_Frame_Drop(struct LMud_Frame* self, LMud_Size count);
