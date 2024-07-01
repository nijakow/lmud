
#pragma once

#include <lmud/defs.h>
#include <lmud/util/stringbuilder.h>

struct LMud_Line
{
    struct LMud_Line*  next;
    char*              text;
};

void LMud_Line_Create(struct LMud_Line* self, const char* text);
void LMud_Line_Destroy(struct LMud_Line* self);

struct LMud_Line*  LMud_Line_New(const char* text);
void               LMud_Line_Delete(struct LMud_Line* self);


struct LMud_Log
{
    struct LMud_Line*  first;
    struct LMud_Line*  last;
};

bool LMud_Log_Create(struct LMud_Log* self);
void LMud_Log_Destroy(struct LMud_Log* self);

void LMud_Log_Append(struct LMud_Log* self, const char* text);


struct LMud_LogComposer
{
    struct LMud_Log*           log;
    struct LMud_StringBuilder  builder;
};

void LMud_LogComposer_Create(struct LMud_LogComposer* self, struct LMud_Log* log);
void LMud_LogComposer_Destroy(struct LMud_LogComposer* self);

void LMud_LogComposer_Commit(struct LMud_LogComposer* self);

void LMud_LogComposer_AppendChar(struct LMud_LogComposer* self, char c);
