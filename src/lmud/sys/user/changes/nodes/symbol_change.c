/**
 * @file symbol_change.c
 * @brief Symbol Change Nodes
 * 
 * This file contains change nodes for tracking symbol manipulation by users.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#include "symbol_change.h"

void LMud_SymbolChangeNode_Create(struct LMud_SymbolChangeNode* self, struct LMud_Symbol* symbol, enum LMud_SymbolSlot slot, LMud_Any old_value, LMud_Any new_value)
{
    self->prev      = NULL;
    self->next      = NULL;
    self->symbol    = symbol;
    self->slot      = slot;
    self->old_value = old_value;
    self->new_value = new_value;
}

void LMud_SymbolChangeNode_Destroy(struct LMud_SymbolChangeNode* self)
{
    LMud_SymbolChangeNode_Unlink(self);
}

void LMud_SymbolChangeNode_Link(struct LMud_SymbolChangeNode* self, struct LMud_SymbolChangeNode** list)
{
    self->prev =  list;
    self->next = *list;

    if (*list != NULL)
        (*list)->prev = &self->next;

    *list = self;
}

void LMud_SymbolChangeNode_Unlink(struct LMud_SymbolChangeNode* self)
{
    if (self->prev != NULL)
        *self->prev = self->next;

    if (self->next != NULL)
        self->next->prev = self->prev;
}
