/**
 * @file console.c
 * @brief The LMud Memory Implementation
 * 
 * This file contains the implementation of the memory system
 * for the LMud project.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#include "memory.h"

void LMud_MemoryReport_Create(struct LMud_MemoryReport* self)
{
    self->objects_allocated = 0;
}

void LMud_MemoryReport_Destroy(struct LMud_MemoryReport* self)
{
    (void) self;
}
