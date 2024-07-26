/**
 * @file changelist.h
 * @brief Change Lists
 * 
 * This file contains change list for tracking changes by users.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#pragma once

#include <lmud/defs.h>


struct LMud_ChangeItem
{
    struct LMud_ChangeItem**  prev;
    struct LMud_ChangeItem*   next;

    /*
     * TODO: Timestamp?
     */
};

void LMud_ChangeItem_Create(struct LMud_ChangeItem* self);
void LMud_ChangeItem_Destroy(struct LMud_ChangeItem* self);
void LMud_ChangeItem_Delete(struct LMud_ChangeItem* self);

void LMud_ChangeItem_Link(struct LMud_ChangeItem* self, struct LMud_ChangeItem** list);
void LMud_ChangeItem_Unlink(struct LMud_ChangeItem* self);


struct LMud_ChangeList
{
    struct LMud_ChangeItem*  items;
};

void LMud_ChangeList_Create(struct LMud_ChangeList* self);
void LMud_ChangeList_Destroy(struct LMud_ChangeList* self);

void* LMud_ChangeList_CreateItem(struct LMud_ChangeList* self, LMud_Size extra_size);
