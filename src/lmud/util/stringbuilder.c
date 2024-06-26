
#include <lmud/util/memory.h>
#include <lmud/util/utf8.h>

#include "stringbuilder.h"


void LMud_StringBuilder_Create(struct LMud_StringBuilder* self)
{
    self->data  = NULL;
    self->alloc = 0;
    self->fill  = 0;
}

void LMud_StringBuilder_Destroy(struct LMud_StringBuilder* self)
{
    LMud_Free(self->data);
}

const char* LMud_StringBuilder_GetStatic(struct LMud_StringBuilder* self)
{
    if (self->data == NULL)
        return "";
    return self->data;
}

void LMud_StringBuilder_Ensure(struct LMud_StringBuilder* self, LMud_Size size)
{
    LMud_Size  new_size;

    if (self->alloc + 1 < size)
    {
        new_size = size + 1;

        if (self->alloc * 2 > new_size)
            new_size = self->alloc * 2;

        self->data = LMud_Realloc(self->data, new_size);
    }
}

void LMud_StringBuilder_AppendChar(struct LMud_StringBuilder* self, char c)
{
    LMud_StringBuilder_Ensure(self, self->fill + 2);
    self->data[self->fill++] = c;
    self->data[self->fill]   = '\0';
}

void LMud_StringBuilder_AppendRune(struct LMud_StringBuilder* self, LMud_Rune rune)
{
    struct LMud_Utf8_Encoder  encoder;

    LMud_Utf8_Encoder_Create(&encoder, rune);
    LMud_StringBuilder_AppendCStr(self, LMud_Utf8_Encoder_AsString(&encoder));
    LMud_Utf8_Encoder_Destroy(&encoder);
}

void LMud_StringBuilder_AppendRune_Uppercased(struct LMud_StringBuilder* self, LMud_Rune rune)
{
    LMud_StringBuilder_AppendRune(self, LMud_Rune_UpperCase(rune));
}

void LMud_StringBuilder_AppendCStr(struct LMud_StringBuilder* self, const char* chars)
{
    LMud_Size  index;

    if (chars != NULL)
    {
        for (index = 0; chars[index] != '\0'; index++)
        {
            LMud_StringBuilder_AppendChar(self, chars[index]);
        }
    }
}

void LMud_StringBuilder_AppendCStr_Uppercased(struct LMud_StringBuilder* self, const char* chars)
{
    struct LMud_Utf8_Decoder  decoder;
    LMud_Size                 index;
    LMud_Rune                 rune;

    LMud_Utf8_Decoder_Create(&decoder);
    {
        for (index = 0; chars[index] != '\0'; index++)
        {
            LMud_Utf8_Decoder_Push(&decoder, chars[index]);

            if (LMud_Utf8_Decoder_GetRune(&decoder, &rune))
            {
                LMud_StringBuilder_AppendRune(self, LMud_Rune_UpperCase(rune));
            }
        }
    }
    LMud_Utf8_Decoder_Destroy(&decoder);
}

void LMud_StringBuilder_AppendSlice(struct LMud_StringBuilder* self, const char* begin, const char* end)
{
    LMud_Size  index;

    for (index = 0; begin + index < end; index++)
    {
        LMud_StringBuilder_AppendChar(self, begin[index]);
    }
}
