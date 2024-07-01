
#include <lmud/lisp/io.h>
#include <lmud/util/memory.h>

#include "frame.h"


void LMud_FrameRef_Create(struct LMud_FrameRef* self, struct LMud_Frame* frame)
{
    struct LMud_FrameExtension*  extension;

    self->frame = frame;
    
    if (frame == NULL) {
        self->next = NULL;
    } else {
        extension = LMud_Frame_EnsureExtension(frame);

        self->next            = extension->references;
        extension->references = self;
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

void LMud_FrameRef_Transfer(struct LMud_FrameRef* self, struct LMud_Frame* frame)
{
    if (self->frame != NULL)
    {
        LMud_Frame_RemoveReference(self->frame, self);
    }

    LMud_FrameRef_Create(self, frame);
}

void LMud_FrameRef_TransferWithoutRemoval(struct LMud_FrameRef* self, struct LMud_Frame* frame)
{
    self->frame = frame;
}


void LMud_FrameExtension_Create(struct LMud_FrameExtension* self, struct LMud_Frame* owner, struct LMud_Frame* lexical)
{
    LMud_FrameRef_Create(&self->lexical, lexical);
    
    self->references = NULL;
    self->return_to  = owner;
}

void LMud_FrameExtension_Destroy(struct LMud_FrameExtension* self)
{
    assert(self->references == NULL);
    LMud_FrameRef_Destroy(&self->lexical);
}

struct LMud_FrameExtension* LMud_FrameExtension_New(struct LMud_Frame* owner, struct LMud_Frame* lexical)
{
    struct LMud_FrameExtension*  self;

    self = LMud_Alloc(sizeof(struct LMud_FrameExtension));

    if (self != NULL)
    {
        LMud_FrameExtension_Create(self, owner, lexical);
    }

    return self;
}

void LMud_FrameExtension_Delete(struct LMud_FrameExtension* self)
{
    LMud_FrameExtension_Destroy(self);
    LMud_Free(self);
}


void LMud_Frame_Create(struct LMud_Frame*    self,
                       struct LMud_Frame*    previous,
                       struct LMud_Frame*    lexical,
                       struct LMud_Function* function,
                       LMud_Any*             arguments,
                       LMud_Size             extra_argument_count)
{
    LMud_Size  index;
    LMud_Size  limit;

    self->previous   = previous;
    self->child      = NULL;
    
    if (lexical == NULL)
        self->extension  = NULL;
    else
        self->extension = LMud_FrameExtension_New(self, lexical);

    self->function       = function;
    self->ip             = 0;
    self->unwind_protect = LMud_UNWIND_PROTECT_UNDEFINED;
    self->sp             = function->info.register_count;
    self->ap             = function->info.register_count + function->info.stack_size + extra_argument_count;
    self->ac             = function->info.register_count + function->info.stack_size;
    self->in_ship        = false;

    for (index = 0; index < function->info.fixed_argument_count; index++)
    {
        self->payload[index] = arguments[index];
    }

    limit = function->info.register_count + function->info.stack_size;
    while (index < limit)
    {
        /*
         * Unfortunately, fetching NIL from the constants is not possible here.
         * This is why all registers are initialized to zero instead.
         */
        self->payload[index++] = LMud_Any_FromInteger(0);
    }

    for (index = 0; index < extra_argument_count; ++index)
    {
        self->payload[function->info.register_count + function->info.stack_size + index] = arguments[function->info.fixed_argument_count + index];
    }
}

void LMud_Frame_Destroy(struct LMud_Frame* self)
{
    if (self->extension != NULL)
    {
        LMud_FrameExtension_Delete(self->extension);
    }
}

void LMud_Frame_Move(struct LMud_Frame* self, struct LMud_Frame* location)
{
    struct LMud_FrameExtension*  extension;
    struct LMud_FrameRef*        reference;

    /*
     * First, we initialize the new frame with the same values as the old frame.
     */
    LMud_CopyMemory(location, self, sizeof(struct LMud_Frame) + LMud_Frame_PayloadSizeInBytes(self));

    /*
     * Transfer all references to the new frame.
     */
    if (self->extension != NULL)
    {
        extension = self->extension;

        for (reference = extension->references; reference != NULL; reference = reference->next)
        {
            LMud_FrameRef_TransferWithoutRemoval(reference, location);
        }
    }

    /*
     * If our 'return_to' slot points to our old frame, we need to update it.
     */
    if (location->extension != NULL && location->extension->return_to == self)
    {
        location->extension->return_to = location;
    }

    /*
     * We have also transferred the extension by copying the pointer.
     * So we need to clear the old pointer to avoid double-freeing.
     */
    self->extension = NULL;

    /*
     * And as the last step, we destroy the old frame.
     */
    LMud_Frame_Destroy(self);
}

struct LMud_FrameExtension* LMud_Frame_EnsureExtension(struct LMud_Frame* self)
{
    if (self->extension == NULL)
    {
        self->extension = LMud_FrameExtension_New(self, NULL);
    }

    return self->extension;
}

struct LMud_Frame* LMud_Frame_GetReturnTo(struct LMud_Frame* self)
{
    if (self->extension == NULL)
        return self;
    else
        return self->extension->return_to;
}

void LMud_Frame_SetReturnTo(struct LMud_Frame* self, struct LMud_Frame* value)
{
    LMud_Frame_EnsureExtension(self)->return_to = value;
}

struct LMud_Frame* LMud_Frame_GetLexical(struct LMud_Frame* self)
{
    if (self->extension == NULL)
        return NULL;
    else
        return LMud_FrameRef_GetFrame(&self->extension->lexical);
}

bool LMud_Frame_ShouldBeMovedToShip(struct LMud_Frame* self)
{
    return !self->in_ship && (self->extension != NULL && self->extension->references != NULL);
}

bool LMud_Frame_IsReadyForShipDeletion(struct LMud_Frame* self)
{
    return self->in_ship && (self->extension == NULL || self->extension->references == NULL) && self->child == NULL;
}

bool LMud_Frame_HasPendingReferences(struct LMud_Frame* self)
{
    return (self->extension != NULL && self->extension->references != NULL);
}

void LMud_Frame_RemoveReference(struct LMud_Frame* self, struct LMud_FrameRef* reference)
{
    struct LMud_FrameRef**  current;

    assert(self->extension != NULL);

    current = &self->extension->references;

    while (*current != NULL)
    {
        if (*current == reference)
        {
            *current = reference->next;
            break;
        }

        current = &(*current)->next;
    }

    /*
     * TODO: If the reference count drops to zero and if we are heap-allocated,
     *       we should free ourselves.
     */
    if (LMud_Frame_IsReadyForShipDeletion(self))
    {
        printf("[Note]: Frame/Ship %p would have been freed here.\n", self);
    }
}


LMud_Size LMud_Frame_PayloadSizeInAnys(struct LMud_Frame* self)
{
    return self->ap;
}

LMud_Size LMud_Frame_PayloadSizeInBytes(struct LMud_Frame* self)
{
    return LMud_Frame_PayloadSizeInAnys(self) * sizeof(LMud_Any);
}


LMud_Size LMud_Frame_ExtraArgumentCount(struct LMud_Frame* self)
{
    return (self->ap - (self->function->info.register_count + self->function->info.stack_size));
}

LMud_Size LMud_Frame_FixedArgumentCount(struct LMud_Frame* self)
{
    return self->function->info.fixed_argument_count;
}

LMud_Size LMud_Frame_GivenArgumentCount(struct LMud_Frame* self)
{
    return LMud_Frame_FixedArgumentCount(self) + LMud_Frame_ExtraArgumentCount(self);
}

LMud_Size LMud_Frame_StackBase(struct LMud_Frame* self)
{
    return self->function->info.register_count;
}


LMud_Any* LMud_Frame_FixedArgumentRef(struct LMud_Frame* self, LMud_Size index)
{
    assert(index < LMud_Frame_FixedArgumentCount(self));
    return &self->payload[index];
}

LMud_Any* LMud_Frame_ExtraArgumentRef(struct LMud_Frame* self, LMud_Size index)
{
    assert(index < LMud_Frame_RemainingExtraArgumentCount(self));
    return &self->payload[self->function->info.register_count + self->function->info.stack_size + index];
}

LMud_Any* LMud_Frame_GivenArgumentRef(struct LMud_Frame* self, LMud_Size index)
{
    LMud_Size  fixed_count;

    fixed_count = LMud_Frame_FixedArgumentCount(self);

    if (index < fixed_count)
        return LMud_Frame_FixedArgumentRef(self, index);
    else
        return LMud_Frame_ExtraArgumentRef(self, index - fixed_count);
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


void LMud_Frame_SetInstructionPointer(struct LMud_Frame* self, uint16_t value)
{
    self->ip = value;
}

void LMud_Frame_SetStackPointerRelativeToPayloadOrigin(struct LMud_Frame* self, LMud_Size offset)
{
    self->sp = offset;
}

void LMud_Frame_SetStackPointerRelativeToStackOrigin(struct LMud_Frame* self, LMud_Size offset)
{
    self->sp = LMud_Frame_StackBase(self) + offset;
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

bool LMud_Frame_GetUnwindProtect(struct LMud_Frame* self, uint16_t* location)
{
    if (self->unwind_protect == LMud_UNWIND_PROTECT_UNDEFINED)
        return false;
    else if (location != NULL)
        *location = self->unwind_protect;
    return true;
}

void LMud_Frame_SetUnwindProtect(struct LMud_Frame* self, uint16_t value)
{
    self->unwind_protect = value;
}

void LMud_Frame_Dump(struct LMud_Frame* self, struct LMud_Lisp* lisp)
{
    LMud_Size  index;
    LMud_Size  limit;
    LMud_Size  space;
    LMud_Size  stack_base;
    bool       sb_pointing;
    bool       sp_pointing;
    bool       ap_pointing;
    bool       ac_pointing;
    bool       any_pointing;

    stack_base = LMud_Frame_StackBase(self);

    printf(" --- Frame %p ---\n", self);
    printf("  Previous:  %p\n", self->previous);
    printf("  Child:     %p\n", self->child);
    printf("  Extension: %p\n", self->extension);
    printf("  Function:  %p\n", self->function);
    printf("  IP: %d\n", self->ip);
    printf("  SB: %lu\n", stack_base);
    printf("  SP: %d\n", self->sp);
    printf("  AP: %d\n", self->ap);
    printf("  AC: %d\n", self->ac);
    printf("  UP: %d\n", self->unwind_protect);
    printf("  In ship: %s\n", self->in_ship ? "yes" : "no");
    printf("\n");
    
    printf("  Payload:\n");

    limit = LMud_Frame_PayloadSizeInAnys(self);

    for (index = 0; index < limit; index++)
    {
        sb_pointing  = (index == stack_base);
        sp_pointing  = (index == self->sp);
        ap_pointing  = (index == self->ap);
        ac_pointing  = (index == self->ac);
        any_pointing = sb_pointing || sp_pointing || ap_pointing || ac_pointing;

        printf("  ");
        if (any_pointing) {
            printf("(");
            if (sb_pointing) printf("B");
            if (sp_pointing) printf("S");
            if (ap_pointing) printf("A");
            if (ac_pointing) printf("C");
            for (space = 0; space < ((LMud_Size) 4) - (sb_pointing + sp_pointing + ap_pointing + ac_pointing); space++)
                printf(" ");
            if (any_pointing) printf(") -->");
        } else {
            printf("          ");
        }
        printf(" [%3lu] ", index);
        LMud_Lisp_Print(lisp, self->payload[index], stdout, true);
        printf("\n");
    }
}


void LMud_FrameShip_Create(struct LMud_FrameShip* self, struct LMud_Frame* frame)
{
    self->prev = NULL;
    self->next = NULL;

    LMud_Frame_Move(frame, &self->frame);
    
    self->frame.in_ship = true;
}

void LMud_FrameShip_Destroy(struct LMud_FrameShip* self)
{
    LMud_FrameShip_Unlink(self);
    LMud_Frame_Destroy(&self->frame);
}

void LMud_FrameShip_Link(struct LMud_FrameShip* self, struct LMud_FrameShip** list)
{
    self->prev =  list;
    self->next = *list;
    
    if (*list != NULL)
        (*list)->prev = &self->next;
    
    *list = self;
}

void LMud_FrameShip_Unlink(struct LMud_FrameShip* self)
{
    if (self->prev != NULL)
        *self->prev = self->next;
    
    if (self->next != NULL)
        self->next->prev = self->prev;
}

struct LMud_FrameShip* LMud_FrameShip_New(struct LMud_Frame* frame)
{
    struct LMud_FrameShip*  self;

    self = LMud_Alloc(sizeof(struct LMud_FrameShip) + LMud_Frame_PayloadSizeInBytes(frame));

    if (self != NULL)
    {
        LMud_FrameShip_Create(self, frame);
    }

    return self;
}

void LMud_FrameShip_Delete(struct LMud_FrameShip* self)
{
    LMud_FrameShip_Destroy(self);
    LMud_Free(self);
}


void LMud_FrameList_Create(struct LMud_FrameList* self)
{
    self->frames = NULL;
}

void LMud_FrameList_Destroy(struct LMud_FrameList* self)
{
    while (self->frames != NULL)
    {
        LMud_FrameShip_Delete(self->frames);
    }
}

struct LMud_FrameShip* LMud_FrameList_Insert(struct LMud_FrameList* self, struct LMud_Frame* frame)
{
    struct LMud_FrameShip*  ship;

    ship = LMud_FrameShip_New(frame);

    if (ship != NULL)
    {
        LMud_FrameShip_Link(ship, &self->frames);
    }

    return ship;
}
