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

    self->status = strdup("");

    return true;
}

void LMud_Console_Destroy(struct LMud_Console* self)
{
    (void) self;
}

static void LMud_Console_EmitCString(struct LMud_Console* self, const char* str)
{
    (void) self;

    printf("%s", str);
}

static void LMud_Console_Emitf(struct LMud_Console* self, const char* fmt, ...)
{
    va_list args;

    (void) self;

    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
}

static void LMud_Console_Flush(struct LMud_Console* self)
{
    (void) self;

    fflush(stdout);
}


unsigned int LMud_Console_GetWidth(struct LMud_Console* self)
{
    return self->width;
}

unsigned int LMud_Console_GetHeight(struct LMud_Console* self)
{
    return self->height;
}

void LMud_Console_Clear(struct LMud_Console* self)
{
    (void) self;

    LMud_Console_EmitCString(self, "\033[2J");
}

void LMud_Console_KillLine(struct LMud_Console* self)
{
    (void) self;

    LMud_Console_EmitCString(self, "\033[K");
}

void LMud_Console_GetCursor(struct LMud_Console* self, unsigned int* x, unsigned int* y)
{
    struct termios  term;
    struct termios  old_term;
    char            buf[32];
    size_t          i;

    i = 0;

    /*
     * Save the current terminal settings.
     */
    tcgetattr(STDIN_FILENO, &old_term);
    term = old_term;

    /*
     * Disable canonical mode and echo.
     */
    term.c_lflag &= ~(ICANON | ECHO);
    tcsetattr(STDIN_FILENO, TCSANOW, &term);

    /*
     * Request cursor position.
     */
    LMud_Console_EmitCString(self, "\033[6n");
    LMud_Console_Flush(self);

    /*
     * Read the response.
     */
    while (i < sizeof(buf) - 1)
    {
        if (read(STDIN_FILENO, &buf[i], 1) != 1)
        {
            break;
        }

        if (buf[i] == 'R')
        {
            break;
        }

        i++;
    }

    buf[i] = '\0';

    /*
     * Restore the terminal settings.
     */
    tcsetattr(STDIN_FILENO, TCSANOW, &old_term);

    /*
     * Parse the response.
     */
    if (sscanf(buf, "\033[%d;%dR", y, x) != 2)
    {
        *x = 0;
        *y = 0;
    }
}

void LMud_Console_MoveCursor(struct LMud_Console* self, unsigned int x, unsigned int y)
{
    LMud_Console_Emitf(self, "\033[%d;%dH", y, x);
}

void LMud_Console_MoveCursorToLine(struct LMud_Console* self, unsigned int line)
{
    LMud_Console_MoveCursor(self, 1, line);
}

void LMud_Console_MoveCursorToBottomLine(struct LMud_Console* self)
{
    LMud_Console_MoveCursorToLine(self, self->height);
}

void LMud_Console_SaveCursor(struct LMud_Console* self, struct LMud_ConsolePosition* pos)
{
    unsigned int x;
    unsigned int y;

    LMud_Console_GetCursor(self, &x, &y);

    pos->x = x;
    pos->y = y;
}

void LMud_Console_RestoreCursor(struct LMud_Console* self, struct LMud_ConsolePosition* pos)
{
    LMud_Console_MoveCursor(self, pos->x, pos->y);
}

void LMud_Console_SaveExcursion(struct LMud_Console* self, struct LMud_ConsoleExcursion* excursion)
{
    LMud_Console_SaveCursor(self, &excursion->pos);
}

void LMud_Console_RestoreExcursion(struct LMud_Console* self, struct LMud_ConsoleExcursion* excursion)
{
    LMud_Console_RestoreCursor(self, &excursion->pos);
}

static const char* LMud_Console_GetSpinner(unsigned int tick)
{
    static const char* spinners[] =
    {
        /*
        "|",
        "/",
        "-",
        "\\"
        */

       // Use braille spinner
       "⣾",
       "⣽",
       "⣻",
       "⢿",
       "⡿",
       "⣟",
       "⣯",
       "⣷"
    };

    return spinners[tick % (sizeof(spinners) / sizeof(spinners[0]))];
}

static void LMud_Console_RedrawStatusLine(struct LMud_Console* self)
{
    struct LMud_ConsoleExcursion  excursion;
    struct LMud_MemoryReport      report;
    time_t                        now;

    now = time(NULL);

    LMud_Console_SaveExcursion(self, &excursion);
    {
        LMud_MemoryReport_Create(&report);
        LMud_Lisp_FetchMemoryReport(LMud_GetLisp(self->lmud), &report);
        LMud_Console_MoveCursorToBottomLine(self);
        LMud_Console_KillLine(self);
        printf(
            "%s %s | %lu objects", 
            LMud_Console_GetSpinner(now),
            self->status,
            report.objects_allocated
        );
        LMud_MemoryReport_Destroy(&report);
    }
    LMud_Console_RestoreExcursion(self, &excursion);
}

void LMud_Console_SetStatus(struct LMud_Console* self, const char* status)
{
    free(self->status);
    self->status = strdup(status);
    LMud_Console_RedrawStatusLine(self);
}

void LMud_Console_Tick(struct LMud_Console* self)
{
    LMud_Console_RedrawStatusLine(self);
}
