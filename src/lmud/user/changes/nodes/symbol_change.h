
#pragma once

#include <lmud/defs.h>
#include <lmud/decls.h>
#include <lmud/lisp/any.h>
#include <lmud/lisp/objects/symbol.h>
#include <lmud/user/changes/changelist.h>

struct LMud_SymbolChangeNode
{
    struct LMud_SymbolChangeNode**  prev;
    struct LMud_SymbolChangeNode*   next;

    struct LMud_Symbol*             symbol;
    enum   LMud_SymbolSlot          slot;
    LMud_Any                        old_value;
    LMud_Any                        new_value;
};

void LMud_SymbolChangeNode_Create(struct LMud_SymbolChangeNode* self, struct LMud_Symbol* symbol, enum LMud_SymbolSlot slot, LMud_Any old_value, LMud_Any new_value);
void LMud_SymbolChangeNode_Destroy(struct LMud_SymbolChangeNode* self);

void LMud_SymbolChangeNode_Link(struct LMud_SymbolChangeNode* self, struct LMud_SymbolChangeNode** list);
void LMud_SymbolChangeNode_Unlink(struct LMud_SymbolChangeNode* self);
