
#include "stream.h"


bool LMud_InputStream_CreateFromString(struct LMud_InputStream* self, const char* data)
{
    self->file_based = false;
    self->file       = NULL;
    self->data       = data;
    self->index      = 0;

    return true;
}

bool LMud_InputStream_CreateFromFile(struct LMud_InputStream* self, FILE* file)
{
    self->file_based = true;
    self->file       = file;
    self->data       = NULL;
    self->index      = 0;

    return true;
}

void LMud_InputStream_Destroy(struct LMud_InputStream* self)
{
    (void) self;
}


bool LMud_InputStream_Eof(struct LMud_InputStream* self)
{
    return (self->file_based) ? feof(self->file) : (self->data[self->index] == '\0');
}

void LMud_InputStream_Advance(struct LMud_InputStream* self)
{
    if (!LMud_InputStream_Eof(self)) {
        if (self->file_based)
            fgetc(self->file);
        else
            self->index++;
    }
}

char LMud_InputStream_Get(struct LMud_InputStream* self)
{
    int  c;

    if (self->file_based) {
        c = fgetc(self->file);
        ungetc(c, self->file);
        return (char) c;
    } else {
        return self->data[self->index];
    }
}

char LMud_InputStream_Read(struct LMud_InputStream* self)
{
    char  c;

    c = LMud_InputStream_Get(self);

    LMud_InputStream_Advance(self);

    return c;
}


bool LMud_InputStream_Check(struct LMud_InputStream* self, char c)
{
    if (LMud_InputStream_Get(self) == c) {
        LMud_InputStream_Advance(self);
        return true;
    } else {
        return false;
    }
}

bool LMud_InputStream_CheckStr(struct LMud_InputStream* self, const char* str)
{
    LMud_Size  saved;
    LMud_Size  index;

    saved = (self->file_based) ? ((LMud_Size) ftell(self->file)) : self->index;

    for (index = 0; str[index] != '\0'; index++)
    {
        if (!LMud_InputStream_Check(self, str[index])) {
            if (self->file_based)
                fseek(self->file, SEEK_SET, (long) saved);
            else
                self->index = saved;
            return false;
        }
    }

    return true;
}

void LMud_InputStream_SkipIf(struct LMud_InputStream* self, LMud_CharPredicate predicate)
{
    while (!LMud_InputStream_Eof(self) && predicate(LMud_InputStream_Get(self)))
    {
        LMud_InputStream_Advance(self);
    }
}
