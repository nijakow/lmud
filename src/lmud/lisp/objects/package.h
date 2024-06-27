
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/lisp/objects/symbol.h>

struct LMud_Package
{
    struct LMud_Package**    prev;
    struct LMud_Package*     next;

    LMud_Any                 name;
    struct LMud_SymbolTable  symbols;
};

void LMud_Package_Create(struct LMud_Package* self, LMud_Any name);
void LMud_Package_Destroy(struct LMud_Package* self);
void LMud_Package_Mark(struct LMud_GC* gc, struct LMud_Package* self);

void LMud_Package_Link(struct LMud_Package* self, struct LMud_Package** list);
void LMud_Package_Unlink(struct LMud_Package* self);

LMud_Any LMud_Package_Name(struct LMud_Package* self);

struct LMud_SymbolTable* LMud_Package_GetSymbolTable(struct LMud_Package* self);
