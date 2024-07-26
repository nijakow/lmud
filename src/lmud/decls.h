/**
 * @file decls.h
 * @brief LMud Declarations
 * 
 * This file contains the root declarations for the LMud project.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#pragma once

#include <lmud/defs.h>
#include <lmud/config.h>
#include <lmud/log/loglevel.h>

struct LMud;
enum   LMud_LogLevel;
struct LMud_Lisp;
struct LMud_Header;
struct LMud_Objects;
struct LMud_Fiber;
struct LMud_Frame;
struct LMud_GC;
struct LMud_GCStats;
struct LMud_Net;
struct LMud_Process;
struct LMud_Profile;
struct LMud_Symbol;


#define LMud_XLogf(lmud, loglevel, format, ...) \
    { \
        if (loglevel >= LMud_HARDCODED_LOG_LEVEL) \
        { \
            LMud_Logf(lmud, loglevel, format, ##__VA_ARGS__); \
        } \
    }
#define LMud_Debugf_Impl(lmud, loglevel, function, file, line, format, ...) LMud_XLogf(lmud, (loglevel), "%s(...) in %s:%d: " format, function, file, line, ##__VA_ARGS__)
#define LMud_Debugf(lmud, loglevel, format, ...) LMud_Debugf_Impl(lmud, (loglevel), __func__, __FILE__, __LINE__, format, ##__VA_ARGS__)

void LMud_Logf(struct LMud* lmud, enum LMud_LogLevel loglevel, const char* format, ...);
