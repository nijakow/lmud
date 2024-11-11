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
#include <lmud/decls.h>

struct LMud_Console
{
    struct LMud*  lmud;

    unsigned int  width;
    unsigned int  height;
};

bool LMud_Console_Create(struct LMud_Console* self, struct LMud* lmud);
void LMud_Console_Destroy(struct LMud_Console* self);
