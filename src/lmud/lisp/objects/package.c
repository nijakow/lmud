
#include <lmud/lisp/gc.h>

#include "package.h"


void LMud_Package_Create(struct LMud_Package* self, LMud_Any name)
{
    self->prev = NULL;
    self->next = NULL;
    self->name = name;
    LMud_SymbolTable_Create(&self->symbols);
}

void LMud_Package_Destroy(struct LMud_Package* self)
{
    LMud_Package_Unlink(self);
    LMud_SymbolTable_Destroy(&self->symbols);
}

void LMud_Package_Mark(struct LMud_GC* gc, struct LMud_Package* self)
{
    LMud_GC_MarkAny(gc, self->name);
    LMud_SymbolTable_Mark(gc, &self->symbols);
}

LMud_Size LMud_Package_CalculateSizeInBytes(struct LMud_Package* self)
{
    (void) self;
    return sizeof(struct LMud_Package);
}

void LMud_Package_Link(struct LMud_Package* self, struct LMud_Package** list)
{
    self->prev = list;
    self->next = *list;
    if (self->next != NULL)
        self->next->prev = &self->next;
    *list = self;
}

void LMud_Package_Unlink(struct LMud_Package* self)
{
    if (self->next != NULL)
        self->next->prev = self->prev;
    if (self->prev != NULL)
        *self->prev = self->next;
}


LMud_Any LMud_Package_Name(struct LMud_Package* self)
{
    return self->name;
}

struct LMud_SymbolTable* LMud_Package_GetSymbolTable(struct LMud_Package* self)
{
    return &self->symbols;
}
