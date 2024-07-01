
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

void LMud_FrameRef_Transfer(struct LMud_FrameRef* self, struct LMud_Frame* frame);
void LMud_FrameRef_TransferWithoutRemoval(struct LMud_FrameRef* self, struct LMud_Frame* frame);


struct LMud_FrameExtension
{
    struct LMud_FrameRef   lexical;
    struct LMud_FrameRef*  references;
    struct LMud_Frame*     return_to;
};

void LMud_FrameExtension_Create(struct LMud_FrameExtension* self, struct LMud_Frame* owner, struct LMud_Frame* lexical);
void LMud_FrameExtension_Destroy(struct LMud_FrameExtension* self);

struct LMud_FrameExtension* LMud_FrameExtension_New(struct LMud_Frame* lexical, struct LMud_Frame* owner);
void                        LMud_FrameExtension_Delete(struct LMud_FrameExtension* self);


struct LMud_Frame
{
    struct LMud_Frame*           previous;
    struct LMud_Frame*           child;
    struct LMud_Function*        function;
    struct LMud_FrameExtension*  extension;
    uint16_t                     ip;
    uint16_t                     unwind_protect;
    uint8_t                      sp;
    uint8_t                      ap;
    uint8_t                      ac;
    bool                         in_ship;
    LMud_Any                     payload[];
};

void LMud_Frame_Create(struct LMud_Frame*    self,
                       struct LMud_Frame*    previous,
                       struct LMud_Frame*    lexical,
                       struct LMud_Function* function,
                       LMud_Any*             arguments,
                       LMud_Size             extra_argument_count);
void LMud_Frame_Destroy(struct LMud_Frame* self);
void LMud_Frame_Move(struct LMud_Frame* self, struct LMud_Frame* location);

struct LMud_FrameExtension* LMud_Frame_EnsureExtension(struct LMud_Frame* self);

struct LMud_Frame* LMud_Frame_GetReturnTo(struct LMud_Frame* self);
void               LMud_Frame_SetReturnTo(struct LMud_Frame* self, struct LMud_Frame* value);

struct LMud_Frame* LMud_Frame_GetLexical(struct LMud_Frame* self);

bool LMud_Frame_ShouldBeMovedToShip(struct LMud_Frame* self);
bool LMud_Frame_IsReadyForShipDeletion(struct LMud_Frame* self);

bool LMud_Frame_HasPendingReferences(struct LMud_Frame* self);
void LMud_Frame_RemoveReference(struct LMud_Frame* self, struct LMud_FrameRef* reference);

LMud_Size LMud_Frame_PayloadSizeInAnys(struct LMud_Frame* self);
LMud_Size LMud_Frame_PayloadSizeInBytes(struct LMud_Frame* self);

LMud_Size LMud_Frame_ExtraArgumentCount(struct LMud_Frame* self);
LMud_Size LMud_Frame_FixedArgumentCount(struct LMud_Frame* self);
LMud_Size LMud_Frame_GivenArgumentCount(struct LMud_Frame* self);

LMud_Size LMud_Frame_StackBase(struct LMud_Frame* self);

LMud_Any* LMud_Frame_FixedArgumentRef(struct LMud_Frame* self, LMud_Size index);
LMud_Any* LMud_Frame_ExtraArgumentRef(struct LMud_Frame* self, LMud_Size index);
LMud_Any* LMud_Frame_GivenArgumentRef(struct LMud_Frame* self, LMud_Size index);

LMud_Size LMud_Frame_RemainingExtraArgumentCount(struct LMud_Frame* self);
bool      LMud_Frame_HasExtraArguments(struct LMud_Frame* self);
bool      LMud_Frame_GetExtraArgument(struct LMud_Frame* self, LMud_Size index, LMud_Any* value);
bool      LMud_Frame_PeekExtraArgument(struct LMud_Frame* self, LMud_Any* value);
bool      LMud_Frame_TakeExtraArgument(struct LMud_Frame* self, LMud_Any* value);
bool      LMud_Frame_PickKeywordArgument(struct LMud_Frame* self, LMud_Any key, LMud_Any* value);

LMud_Any LMud_Frame_GetRegister(struct LMud_Frame* self, LMud_Size index);
void     LMud_Frame_SetRegister(struct LMud_Frame* self, LMud_Size index, LMud_Any value);

void      LMud_Frame_SetInstructionPointer(struct LMud_Frame* self, uint16_t value);

void      LMud_Frame_SetStackPointerRelativeToPayloadOrigin(struct LMud_Frame* self, LMud_Size offset);
void      LMud_Frame_SetStackPointerRelativeToStackOrigin(struct LMud_Frame* self, LMud_Size offset);
void      LMud_Frame_Push(struct LMud_Frame* self, LMud_Any value);
LMud_Any  LMud_Frame_Pop(struct LMud_Frame* self);
LMud_Any* LMud_Frame_PopN(struct LMud_Frame* self, LMud_Size count);
void      LMud_Frame_Drop(struct LMud_Frame* self, LMud_Size count);

bool LMud_Frame_GetUnwindProtect(struct LMud_Frame* self, uint16_t* location);
void LMud_Frame_SetUnwindProtect(struct LMud_Frame* self, uint16_t value);

void LMud_Frame_Dump(struct LMud_Frame* self, struct LMud_Lisp* lisp);


struct LMud_FrameShip
{
    struct LMud_FrameShip**  prev;
    struct LMud_FrameShip*   next;
    struct LMud_Frame        frame;
};

void LMud_FrameShip_Create(struct LMud_FrameShip* self, struct LMud_Frame* frame);
void LMud_FrameShip_Destroy(struct LMud_FrameShip* self);

struct LMud_FrameShip* LMud_FrameShip_New(struct LMud_Frame* frame);
void LMud_FrameShip_Delete(struct LMud_FrameShip* self);

void LMud_FrameShip_Link(struct LMud_FrameShip* self, struct LMud_FrameShip** list);
void LMud_FrameShip_Unlink(struct LMud_FrameShip* self);


struct LMud_FrameList
{
    struct LMud_FrameShip*  frames;
};

void LMud_FrameList_Create(struct LMud_FrameList* self);
void LMud_FrameList_Destroy(struct LMud_FrameList* self);

struct LMud_FrameShip* LMud_FrameList_Insert(struct LMud_FrameList* self, struct LMud_Frame* frame);
