
#pragma once

#include <lmud/defs.h>

void* LMud_Alloc(LMud_Size size);
void* LMud_Realloc(void* ptr, LMud_Size size);
void  LMud_Free(void* ptr);
