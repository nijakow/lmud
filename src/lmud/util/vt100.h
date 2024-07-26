/**
 * @file vt100.h
 * @brief VT100 Terminal Codes
 * 
 * This file contains VT100 terminal codes for text formatting.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#pragma once

#include <lmud/defs.h>

#define LMud_VT100_Normal      "\x1b[0m"
#define LMud_VT100_Bold        "\x1b[1m"
#define LMud_VT100_Italic      "\x1b[3m"
#define LMud_VT100_Underline   "\x1b[4m"
#define LMud_VT100_Blink       "\x1b[5m"
#define LMud_VT100_Reverse     "\x1b[7m"

#define LMud_VT100_Black       "\x1b[30m"
#define LMud_VT100_Red         "\x1b[31m"
#define LMud_VT100_Green       "\x1b[32m"
#define LMud_VT100_Yellow      "\x1b[33m"
#define LMud_VT100_Blue        "\x1b[34m"
#define LMud_VT100_Magenta     "\x1b[35m"
#define LMud_VT100_Cyan        "\x1b[36m"
#define LMud_VT100_White       "\x1b[37m"
