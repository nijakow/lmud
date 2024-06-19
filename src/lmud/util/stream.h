
#pragma once

#include <lmud/defs.h>


struct LMud_InputStream
{
    const char*  data;
    LMud_Size    index;
};

bool LMud_InputStream_Create(struct LMud_InputStream* self, const char* data);
void LMud_InputStream_Destroy(struct LMud_InputStream* self);

bool LMud_InputStream_Eof(struct LMud_InputStream* self);

void LMud_InputStream_Advance(struct LMud_InputStream* self);
char LMud_InputStream_Get(struct LMud_InputStream* self);
char LMud_InputStream_Read(struct LMud_InputStream* self);

bool LMud_InputStream_Check(struct LMud_InputStream* self, char c);
bool LMud_InputStream_CheckStr(struct LMud_InputStream* self, const char* str);
