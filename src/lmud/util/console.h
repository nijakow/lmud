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

struct LMud_ConsolePosition
{
    unsigned int x;
    unsigned int y;
};

struct LMud_ConsoleExcursion
{
    struct LMud_ConsolePosition pos;
};


struct LMud_Console
{
    struct LMud*  lmud;

    unsigned int  width;
    unsigned int  height;
};

bool LMud_Console_Create(struct LMud_Console* self, struct LMud* lmud);
void LMud_Console_Destroy(struct LMud_Console* self);

unsigned int LMud_Console_GetWidth(struct LMud_Console* self);
unsigned int LMud_Console_GetHeight(struct LMud_Console* self);

void LMud_Console_Clear(struct LMud_Console* self);
void LMud_Console_KillLine(struct LMud_Console* self);

void LMud_Console_GetCursor(struct LMud_Console* self, unsigned int* x, unsigned int* y);

void LMud_Console_MoveCursor(struct LMud_Console* self, unsigned int x, unsigned int y);
void LMud_Console_MoveCursorToLine(struct LMud_Console* self, unsigned int line);
void LMud_Console_MoveCursorToBottomLine(struct LMud_Console* self);

void LMud_Console_SaveCursor(struct LMud_Console* self, struct LMud_ConsolePosition* pos);
void LMud_Console_RestoreCursor(struct LMud_Console* self, struct LMud_ConsolePosition* pos);

void LMud_Console_SaveExcursion(struct LMud_Console* self, struct LMud_ConsoleExcursion* excursion);
void LMud_Console_RestoreExcursion(struct LMud_Console* self, struct LMud_ConsoleExcursion* excursion);

void LMud_Console_Tick(struct LMud_Console* self);
