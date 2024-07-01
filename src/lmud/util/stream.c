
#include <lmud/log/log.h>
#include <lmud/util/stringbuilder.h>
#include <lmud/util/utf8.h>

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

static void LMud_InputStream_Unread(struct LMud_InputStream* self, char c)
{
    if (self->file_based) {
        ungetc(c, self->file);
    }
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
        LMud_InputStream_Unread(self, c);
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
    char       c;
    char       pushbacks[1024];

    if (self->file_based) {
        saved = 0;

        for (index = 0; str[index] != '\0'; index++)
        {
            c = LMud_InputStream_Read(self);

            pushbacks[saved++] = c;
            
            if (c != str[index]) {
                while (saved --> 0)
                {
                    LMud_InputStream_Unread(self, pushbacks[saved]);
                }

                return false;
            }
        }
    } else {
        saved = self->index;

        for (index = 0; str[index] != '\0'; index++)
        {
            if (!LMud_InputStream_Check(self, str[index])) {
                self->index = saved;
                return false;
            }
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



static void LMud_OutputStream_FputcWrapper(FILE* file, char c)
{
    fputc(c, file);
}


void LMud_OutputStream_Create(struct LMud_OutputStream* self, void* payload, LMud_OutputStreamWrite write)
{
    self->payload = payload;
    self->write   = write;
}

void LMud_OutputStream_CreateOnFile(struct LMud_OutputStream* self, FILE* file)
{
    LMud_OutputStream_Create(self, file, (LMud_OutputStreamWrite) LMud_OutputStream_FputcWrapper);
}

void LMud_OutputStream_CreateOnStringBuilder(struct LMud_OutputStream* self, struct LMud_StringBuilder* builder)
{
    LMud_OutputStream_Create(self, builder, (LMud_OutputStreamWrite) LMud_StringBuilder_AppendChar);
}

void LMud_OutputStream_CreateOnLogComposer(struct LMud_OutputStream* self, struct LMud_LogComposer* composer)
{
    LMud_OutputStream_Create(self, composer, (LMud_OutputStreamWrite) LMud_LogComposer_AppendChar);
}

void LMud_OutputStream_Destroy(struct LMud_OutputStream* self)
{
    (void) self;
}

void LMud_OutputStream_WriteChar(struct LMud_OutputStream* self, char c)
{
    self->write(self->payload, c);
}

void LMud_OutputStream_WriteRune(struct LMud_OutputStream* self, LMud_Rune rune)
{
    struct LMud_Utf8_Encoder  encoder;
    
    LMud_Utf8_Encoder_Create(&encoder, rune);
    LMud_OutputStream_WriteCString(self, LMud_Utf8_Encoder_AsString(&encoder));
    LMud_Utf8_Encoder_Destroy(&encoder);
}

void LMud_OutputStream_WriteCString(struct LMud_OutputStream* self, const char* str)
{
    LMud_Size  index;

    for (index = 0; str[index] != '\0'; index++)
    {
        LMud_OutputStream_WriteChar(self, str[index]);
    }
}

void LMud_OutputStream_Printf(struct LMud_OutputStream* self, const char* format, ...)
{
    va_list    args;
    int        result;
    LMud_Size  length;
    char       buffer[4096];

    va_start(args, format);
    result = vsnprintf(buffer, sizeof(buffer), format, args);
    va_end(args);

    length = (LMud_Size) result;

    if (result >= 0 && length > 0 && length < sizeof(buffer))
    {
        LMud_OutputStream_WriteCString(self, buffer);
    }
}
