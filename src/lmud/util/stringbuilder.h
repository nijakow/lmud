/**
 * @file stringbuilder.h
 * @brief An Efficient String Builder
 * 
 * This file contains a string builder implementation
 * to concatenate strings efficiently.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#pragma once

#include <lmud/defs.h>

struct LMud_StringBuilder
{
    char*      data;
    LMud_Size  alloc;
    LMud_Size  fill;
};

void LMud_StringBuilder_Create(struct LMud_StringBuilder* self);
void LMud_StringBuilder_Destroy(struct LMud_StringBuilder* self);

const char* LMud_StringBuilder_GetStatic(struct LMud_StringBuilder* self);

void LMud_StringBuilder_AppendChar(struct LMud_StringBuilder* self, char c);
void LMud_StringBuilder_AppendRune(struct LMud_StringBuilder* self, LMud_Rune rune);
void LMud_StringBuilder_AppendRune_Uppercased(struct LMud_StringBuilder* self, LMud_Rune rune);
void LMud_StringBuilder_AppendCStr(struct LMud_StringBuilder* self, const char* chars);
void LMud_StringBuilder_AppendCStr_Uppercased(struct LMud_StringBuilder* self, const char* chars);
void LMud_StringBuilder_AppendSlice(struct LMud_StringBuilder* self, const char* begin, const char* end);
