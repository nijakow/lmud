
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


bool LMud_String_Equals(struct LMud_String* self, const char* value)
{
    LMud_Size  index;

    index = 0;

    while (value[index] == self->chars[index])
    {
        if (value[index] == '\0')
            return true;
        index++;
    }

    return false;
}
