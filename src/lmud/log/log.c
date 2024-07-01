
#include <lmud/util/memory.h>

#include "log.h"

void LMud_Line_Create(struct LMud_Line* self, const char* text)
{
    self->next = NULL;
    self->text = LMud_Strdup(text);
}

void LMud_Line_Destroy(struct LMud_Line* self)
{
    LMud_Free(self->text);
}

struct LMud_Line* LMud_Line_New(const char* text)
{
    struct LMud_Line*  line;
    
    line = LMud_Alloc(sizeof(struct LMud_Line));
    
    if (line != NULL)
    {
        LMud_Line_Create(line, text);
    }

    return line;
}

void LMud_Line_Delete(struct LMud_Line* self)
{
    LMud_Line_Destroy(self);
    LMud_Free(self);
}


bool LMud_Log_Create(struct LMud_Log* self)
{
    self->first = NULL;
    self->last  = NULL;

    return true;
}

void LMud_Log_Destroy(struct LMud_Log* self)
{
    struct LMud_Line*  line;
    struct LMud_Line*  next;
    
    line = self->first;
    
    while (line != NULL)
    {
        next = line->next;
        
        LMud_Line_Destroy(line);
        LMud_Free(line);
        
        line = next;
    }
}

void LMud_Log_Append(struct LMud_Log* self, const char* text)
{
    struct LMud_Line*  line;
    
    line = LMud_Line_New(text);
    
    if (line != NULL)
    {
        if (self->first == NULL)
        {
            self->first = line;
        }
        else
        {
            self->last->next = line;
        }
        
        self->last = line;
    }
}


static void LMud_LogComposer_FreshLine(struct LMud_LogComposer* self)
{
    LMud_StringBuilder_AppendCStr(&self->builder, "; ");
}

void LMud_LogComposer_Create(struct LMud_LogComposer* self, struct LMud_Log* log, enum LMud_LogLevel loglevel)
{
    self->log         = log;
    self->loglevel    = loglevel;
    self->line_offset = 0;
    LMud_StringBuilder_Create(&self->builder);
}

void LMud_LogComposer_Destroy(struct LMud_LogComposer* self)
{
    LMud_StringBuilder_Destroy(&self->builder);
}

void LMud_LogComposer_Commit(struct LMud_LogComposer* self)
{
    if (self->line_offset > 0)
    {
        LMud_StringBuilder_AppendChar(&self->builder, '\n');
    }
    printf("%s", self->builder.data);
}

void LMud_LogComposer_AppendChar(struct LMud_LogComposer* self, char c)
{
    if (self->line_offset == 0)
    {
        LMud_LogComposer_FreshLine(self);
    }

    self->line_offset = (c == '\n') ? 0 : self->line_offset + 1;

    LMud_StringBuilder_AppendChar(&self->builder, c);
}
