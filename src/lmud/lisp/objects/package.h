
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/lisp/objects/symbol.h>

struct LMud_Package
{
    struct LMud_Object       _;

    struct LMud_SymbolTable  symbols;
};

void LMud_Package_Create(struct LMud_Package* self);
void LMud_Package_Destroy(struct LMud_Package* self);
