
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
#define LMud_RELEASE_NAME "Patchy Phoenix"

typedef size_t        LMud_Size;
typedef int           LMud_Integer;
typedef unsigned int  LMud_Rune;

struct LMud_Lisp;
struct LMud_Header;
struct LMud_Objects;
struct LMud_Fiber;
struct LMud_Frame;
struct LMud_GC;
