/**
 * @file memory.h
 * @brief The LMud Memory Utilities Header
 * 
 * This file contains a thin wrapper around the libc memory allocation
 * and string manipulation functions for the LMud project.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#pragma once

#include <lmud/defs.h>

#ifdef LMud_ENABLE_MALLOC_TRIM
#include <malloc.h>
#endif


void* LMud_Alloc(LMud_Size size);
void* LMud_Realloc(void* ptr, LMud_Size size);
void  LMud_Free(void* ptr);

void LMud_TrimMalloc();

LMud_Size LMud_CStr_Length(const char* str);
bool      LMud_CStr_Equals(const char* a, const char* b);
bool      LMud_CStr_EqualsIgnoreCase(const char* a, const char* b);

bool LMud_CStr_FindAndPartition(const char* cstr, const char* separator, const char** end, const char** after);

char* LMud_Strdup(const char* str);

void LMud_CopyMemory(void* dest, const void* src, LMud_Size size);
