
#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include <assert.h>
#include <stdarg.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <ctype.h>


#define LMud_VERSION "0.1"
#define LMud_VERSION_EXTRA "alpha"
#define LMud_RELEASE_NAME "Glitchy Gargoyle"

#define LMud_SYMBOL_NAME_LENGTH 1023


typedef size_t LMud_Size;

typedef int LMud_Integer;


struct LMud_Lisp;
struct LMud_Object;
struct LMud_Objects;
struct LMud_Fiber;
struct LMud_Frame;
