
#pragma once

#include <lmud/lisp/base.h>

struct LMud_String
{
    LMud_Size           byte_size;
    char*               chars;
    char                payload[];
};

void LMud_String_Create_Overallocated(struct LMud_String* self, const char* chars);
void LMud_String_Destroy(struct LMud_String* self);
void LMud_String_Mark(struct LMud_GC* gc, struct LMud_String* self);
LMud_Size LMud_String_CalculateSizeInBytes(struct LMud_String* self);

const char* LMud_String_Chars(struct LMud_String* self);

bool LMud_String_IsEmpty(struct LMud_String* self);
bool LMud_String_Equals(struct LMud_String* self, const char* value);

bool      LMud_String_RuneAt(struct LMud_String* self, size_t index, LMud_Rune* result);
LMud_Size LMud_String_RuneLength(struct LMud_String* self);
