/**
 * @file console.c
 * @brief The LMud Console Utilities Implementation
 * 
 * This file contains the implementation of the console utilities
 * for the LMud project.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#include <lmud/glue.h>

#include "console.h"

bool LMud_Console_Create(struct LMud_Console* self, struct LMud* lmud)
{
    struct winsize w;

    self->lmud = lmud;

    /*
     * Get the width and height of the console (terminal).
     */
    ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);

    self->width  = w.ws_col;
    self->height = w.ws_row;

    LMud_Logf(self->lmud, LMud_LogLevel_DEBUG, "Console size: %dx%d", self->width, self->height);

    return true;
}

void LMud_Console_Destroy(struct LMud_Console* self)
{
    (void) self;
}
