
#include <lmud/util/memory.h>

#include "utf8.h"


static void LMud_Utf8_Encoder_Append(struct LMud_Utf8_Encoder* self, char byte)
{
    /*
     * TODO: Protect against buffer overflow.
     */
    self->buffer[self->write++] = byte;
    self->buffer[self->write]   = '\0';
}

void LMud_Utf8_Encoder_Create(struct LMud_Utf8_Encoder* self, LMud_Rune rune)
{
    self->read      = 0;
    self->write     = 0;
    self->buffer[0] = '\0';

    if (rune < 0x80) {
        LMud_Utf8_Encoder_Append(self, rune);
    } else if (rune < 0x800) {
        LMud_Utf8_Encoder_Append(self, 0xC0 | (rune >> 6));
        LMud_Utf8_Encoder_Append(self, 0x80 | (rune & 0x3F));
    } else if (rune < 0x10000) {
        LMud_Utf8_Encoder_Append(self, 0xE0 | (rune >> 12));
        LMud_Utf8_Encoder_Append(self, 0x80 | ((rune >> 6) & 0x3F));
        LMud_Utf8_Encoder_Append(self, 0x80 | (rune & 0x3F));
    } else {
        LMud_Utf8_Encoder_Append(self, 0xF0 | (rune >> 18));
        LMud_Utf8_Encoder_Append(self, 0x80 | ((rune >> 12) & 0x3F));
        LMud_Utf8_Encoder_Append(self, 0x80 | ((rune >> 6) & 0x3F));
        LMud_Utf8_Encoder_Append(self, 0x80 | (rune & 0x3F));
    }
}

void LMud_Utf8_Encoder_Destroy(struct LMud_Utf8_Encoder* self)
{
    (void) self;
}

bool LMud_Utf8_Encoder_HasBytesToRead(struct LMud_Utf8_Encoder* self)
{
    return self->read < self->write;
}

bool LMud_Utf8_Encoder_Read(struct LMud_Utf8_Encoder* self, char* byte)
{
    /*
     * TODO: Protect against overflow.
     */
    if (LMud_Utf8_Encoder_HasBytesToRead(self))
    {
        *byte = self->buffer[self->read++];
        return true;
    }

    return false;
}

const char* LMud_Utf8_Encoder_AsString(struct LMud_Utf8_Encoder* self)
{
    return &self->buffer[self->read];
}


void LMud_Utf8_Decoder_Create(struct LMud_Utf8_Decoder* self)
{
    self->remaining    = 0;
    self->current_rune = 0;
    self->blocked      = true;
}

void LMud_Utf8_Decoder_Destroy(struct LMud_Utf8_Decoder* self)
{
    (void) self;
}

bool LMud_Utf8_Decoder_IsComplete(struct LMud_Utf8_Decoder* self)
{
    return (self->remaining == 0) && (!self->blocked);
}

void LMud_Utf8_Decoder_Push(struct LMud_Utf8_Decoder* self, char byte)
{
    self->blocked = false;

    if (self->remaining == 0) {
        if ((byte & 0x80) == 0) {
            self->current_rune = byte;
        } else if ((byte & 0xE0) == 0xC0) {
            self->current_rune = byte & 0x1F;
            self->remaining    = 1;
        } else if ((byte & 0xF0) == 0xE0) {
            self->current_rune = byte & 0x0F;
            self->remaining    = 2;
        } else if ((byte & 0xF8) == 0xF0) {
            self->current_rune = byte & 0x07;
            self->remaining    = 3;
        } else {
            self->current_rune = 0;
        }
    } else {
        self->current_rune = (self->current_rune << 6) | (byte & 0x3F);
        self->remaining--;
    }
}

bool LMud_Utf8_Decoder_GetRune(struct LMud_Utf8_Decoder* self, LMud_Rune* rune)
{
    if (LMud_Utf8_Decoder_IsComplete(self))
    {
        *rune = self->current_rune;
        return true;
    }

    return false;
}


LMud_Rune LMud_Rune_FromChar(char byte)
{
    return (LMud_Rune) byte;
}

LMud_Rune LMud_Rune_FromInteger(LMud_Integer value)
{
    return (LMud_Rune) value;
}

char LMud_Rune_AsChar(LMud_Rune rune)
{
    return (char) rune;
}

LMud_Integer LMud_Rune_AsInteger(LMud_Rune rune)
{
    return (LMud_Integer) rune;
}


LMud_Rune LMud_Utf8_UpperCase(LMud_Rune rune)
{
    if (rune >= 0x61 && rune <= 0x7A) {
        return rune - 0x20;
    }

    return rune;
}

LMud_Rune LMud_Utf8_LowerCase(LMud_Rune rune)
{
    if (rune >= 0x41 && rune <= 0x5A) {
        return rune + 0x20;
    }

    return rune;
}

bool LMud_Rune_ByName(const char* name, LMud_Rune* rune)
{
    struct LMud_Utf8_Decoder  decoder;
    char*                     ptr;
    LMud_Rune                 result;
    bool                      success;

    ptr     = (char*) name;
    success = true;

    if (LMud_CStr_EqualsIgnoreCase(ptr, "space"))
        result = ' ';
    else if (LMud_CStr_EqualsIgnoreCase(ptr, "newline"))
        result = '\n';
    else if (LMud_CStr_EqualsIgnoreCase(ptr, "return"))
        result = '\r';
    else if (LMud_CStr_EqualsIgnoreCase(ptr, "tab"))
        result = '\t';
    else {
        LMud_Utf8_Decoder_Create(&decoder);
        {
            while (*ptr != '\0' && !LMud_Utf8_Decoder_IsComplete(&decoder))
            {
                LMud_Utf8_Decoder_Push(&decoder, *ptr);
                ptr++;
            }

            success = LMud_Utf8_Decoder_GetRune(&decoder, &result) && (*ptr == '\0');
        }
        LMud_Utf8_Decoder_Destroy(&decoder);
    }

    *rune = result;

    return success;
}

const char* LMud_Rune_Name(LMud_Rune rune)
{
    switch (rune)
    {
        case ' ':  return "Space";
        case '\n': return "Newline";
        case '\r': return "Return";
        case '\t': return "Tab";
        default:   return NULL;
    }
}

bool LMud_Rune_IsPrintable(LMud_Rune rune)
{
    return (rune >= 0x20 && rune <= 0x7E) || (rune >= 0xA0);
}
