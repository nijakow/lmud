/**
 * @file console.h
 * @brief The LMud Memory Header
 * 
 * This file contains the memory system for the LMud project.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#pragma once

#include <lmud/defs.h>

struct LMud_MemoryReport
{
    LMud_Size  objects_allocated;
};

void LMud_MemoryReport_Create(struct LMud_MemoryReport* self);
void LMud_MemoryReport_Destroy(struct LMud_MemoryReport* self);
