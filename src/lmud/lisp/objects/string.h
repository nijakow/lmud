
#pragma once

#include <lmud/lisp/base.h>

struct LMud_String
{
    struct LMud_Object  _;

    char*               chars;
    char                payload[];
};

void LMud_String_Create(struct LMud_String* self, const char* chars);
void LMud_String_Destroy(struct LMud_String* self);

const char* LMud_String_Chars(struct LMud_String* self);

bool LMud_String_Equals(struct LMud_String* self, const char* value);

bool      LMud_String_RuneAt(struct LMud_String* self, size_t index, LMud_Rune* result);
LMud_Size LMud_String_RuneLength(struct LMud_String* self);
