
#pragma once

#include <lmud/defs.h>

struct LMud_StringBuilder
{
    char*      data;
    LMud_Size  alloc;
    LMud_Size  fill;
};

void LMud_StringBuilder_Create(struct LMud_StringBuilder* self);
void LMud_StringBuilder_Destroy(struct LMud_StringBuilder* self);

const char* LMud_StringBuilder_GetStatic(struct LMud_StringBuilder* self);

void LMud_StringBuilder_AppendChar(struct LMud_StringBuilder* self, char c);
void LMud_StringBuilder_AppendCStr(struct LMud_StringBuilder* self, const char* chars);
