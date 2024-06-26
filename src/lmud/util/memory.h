
#pragma once

#include <lmud/defs.h>

void* LMud_Alloc(LMud_Size size);
void* LMud_Realloc(void* ptr, LMud_Size size);
void  LMud_Free(void* ptr);

LMud_Size LMud_CStr_Length(const char* str);
bool      LMud_CStr_Equals(const char* a, const char* b);
bool      LMud_CStr_EqualsIgnoreCase(const char* a, const char* b);

char* LMud_Strdup(const char* str);

void LMud_CopyMemory(void* dest, const void* src, LMud_Size size);
