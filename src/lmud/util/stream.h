
#pragma once

#include <lmud/defs.h>


typedef bool (*LMud_CharPredicate)(char c);


struct LMud_InputStream
{
    bool         file_based;
    FILE*        file;
    const char*  data;
    LMud_Size    index;
};

bool LMud_InputStream_CreateFromString(struct LMud_InputStream* self, const char* data);
bool LMud_InputStream_CreateFromFile(struct LMud_InputStream* self, FILE* file);
void LMud_InputStream_Destroy(struct LMud_InputStream* self);

bool LMud_InputStream_Eof(struct LMud_InputStream* self);

void LMud_InputStream_Advance(struct LMud_InputStream* self);
char LMud_InputStream_Get(struct LMud_InputStream* self);
char LMud_InputStream_Read(struct LMud_InputStream* self);

bool LMud_InputStream_Check(struct LMud_InputStream* self, char c);
bool LMud_InputStream_CheckStr(struct LMud_InputStream* self, const char* str);

void LMud_InputStream_SkipIf(struct LMud_InputStream* self, LMud_CharPredicate predicate);
