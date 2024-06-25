
#include <lmud/util/memory.h>
#include <lmud/util/utf8.h>

#include "string.h"

void LMud_String_Create(struct LMud_String* self, const char* chars)
{
    (void) self;
    (void) chars;
}

void LMud_String_Destroy(struct LMud_String* self)
{
    (void) self;
}


const char* LMud_String_Chars(struct LMud_String* self)
{
    return self->chars;
}

bool LMud_String_Equals(struct LMud_String* self, const char* value)
{
    return LMud_CStr_Equals(self->chars, value);
}

bool LMud_String_RuneAt(struct LMud_String* self, size_t index, LMud_Rune* result)
{
    struct LMud_Utf8_Decoder  decoder;
    LMud_Size                 current;
    char*                     iterator;
    bool                      success;

    success = false;

    LMud_Utf8_Decoder_Create(&decoder);
    for (iterator = self->chars, current = 0; *iterator != '\0'; iterator++)
    {
        LMud_Utf8_Decoder_Push(&decoder, *iterator);

        if (LMud_Utf8_Decoder_GetRune(&decoder, result)) {
            if (current == index) {
                success = true;
                break;
            } else {
                current++;
            }
        }
    }
    LMud_Utf8_Decoder_Destroy(&decoder);

    return success;
}

LMud_Size LMud_String_RuneLength(struct LMud_String* self)
{
    struct LMud_Utf8_Decoder  decoder;
    char*                     iterator;
    LMud_Size                 length;

    LMud_Utf8_Decoder_Create(&decoder);
    for (iterator = self->chars, length = 0; *iterator != '\0'; iterator++)
    {
        LMud_Utf8_Decoder_Push(&decoder, *iterator);

        if (LMud_Utf8_Decoder_IsComplete(&decoder)) {
            length++;
        }
    }
    LMud_Utf8_Decoder_Destroy(&decoder);

    return length;
}
