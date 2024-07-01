
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


typedef void (*LMud_OutputStreamWrite)(void* payload, char c);

struct LMud_StringBuilder;
struct LMud_LogComposer;


struct LMud_OutputStream
{
    void*                   payload;
    LMud_OutputStreamWrite  write;
};

void LMud_OutputStream_Create(struct LMud_OutputStream* self, void* payload, LMud_OutputStreamWrite write);
void LMud_OutputStream_CreateOnFile(struct LMud_OutputStream* self, FILE* file);
void LMud_OutputStream_CreateOnStringBuilder(struct LMud_OutputStream* self, struct LMud_StringBuilder* builder);
void LMud_OutputStream_CreateOnLogComposer(struct LMud_OutputStream* self, struct LMud_LogComposer* composer);
void LMud_OutputStream_Destroy(struct LMud_OutputStream* self);

void LMud_OutputStream_WriteChar(struct LMud_OutputStream* self, char c);
void LMud_OutputStream_WriteRune(struct LMud_OutputStream* self, LMud_Rune rune);
void LMud_OutputStream_WriteCString(struct LMud_OutputStream* self, const char* str);
void LMud_OutputStream_Printf(struct LMud_OutputStream* self, const char* format, ...);
