/**
 * @file changelist.c
 * @brief Change Lists
 * 
 * This file contains change list for tracking changes by users.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#include <lmud/util/memory.h>

#include "changelist.h"


void LMud_ChangeItem_Create(struct LMud_ChangeItem* self)
{
    self->prev = NULL;
    self->next = NULL;
}

void LMud_ChangeItem_Destroy(struct LMud_ChangeItem* self)
{
    LMud_ChangeItem_Unlink(self);
}

void LMud_ChangeItem_Delete(struct LMud_ChangeItem* self)
{
    LMud_ChangeItem_Destroy(self);
    LMud_Free(self);
}

void LMud_ChangeItem_Link(struct LMud_ChangeItem* self, struct LMud_ChangeItem** list)
{
    LMud_ChangeItem_Unlink(self);

    self->prev =  list;
    self->next = *list;

    if (*list != NULL)
        (*list)->prev = &self->next;

    *list = self;
}

void LMud_ChangeItem_Unlink(struct LMud_ChangeItem* self)
{
    if (self->prev != NULL)
        *self->prev = self->next;

    if (self->next != NULL)
        self->next->prev = self->prev;

    self->prev = NULL;
    self->next = NULL;
}


void LMud_ChangeList_Create(struct LMud_ChangeList* self)
{
    self->items = NULL;
}

void LMud_ChangeList_Destroy(struct LMud_ChangeList* self)
{
    while (self->items != NULL)
    {
        LMud_ChangeItem_Delete(self->items);
    }
}

void* LMud_ChangeList_CreateItem(struct LMud_ChangeList* self, LMud_Size extra_size)
{
    struct LMud_ChangeItem*  item;
    
    item = (struct LMud_ChangeItem*) LMud_Alloc(sizeof(struct LMud_ChangeItem) + extra_size);

    if (item != NULL)
    {
        LMud_ChangeItem_Create(item);
        LMud_ChangeItem_Link(item, &self->items);
    }

    return item;
}
