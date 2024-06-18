
#pragma once

#include <lmud/lisp/lisp.h>

struct LMud_String
{
    struct LMud_Object  _;

    char*               chars;
    char                payload[];
};

void LMud_String_Create(struct LMud_String* self, const char* chars);
void LMud_String_Destroy(struct LMud_String* self);
