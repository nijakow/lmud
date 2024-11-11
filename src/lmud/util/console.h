/**
 * @file console.h
 * @brief The LMud Console Utilities Header
 * 
 * This file contains console utilities for the LMud project.
 * Essentially, this is a micro-implementation of ncurses.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#pragma once

#include <lmud/defs.h>

struct LMud_Console
{

};

void LMud_Console_Create(struct LMud_Console* self);
void LMud_Console_Destroy(struct LMud_Console* self);
