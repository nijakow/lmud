/**
 * @file config.h
 * @brief LMud Build Configuration
 * 
 * This file contains the build flags and configuration options for the LMud project.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#pragma once

#define LMud_VERSION "0.1"
#define LMud_VERSION_EXTRA "beta"
#define LMud_RELEASE_NAME "Beta Banana"

#define LMud_ENABLE_COMPRESSED_ANYS

#ifdef __GLIBC__
#  define LMud_ENABLE_MALLOC_TRIM
#endif

#define LMud_UNWIND_PROTECT_UNDEFINED   0xffff
#define LMud_UNWIND_PROTECT_MAX_NESTING 8

#define LMud_HARDCODED_LOG_LEVEL LMud_LogLevel_WARNING
