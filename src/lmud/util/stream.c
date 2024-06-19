
#include "stream.h"


bool LMud_InputStream_Create(struct LMud_InputStream* self, const char* data)
{
    self->data  = data;
    self->index = 0;

    return true;
}

void LMud_InputStream_Destroy(struct LMud_InputStream* self)
{
    (void) self;
}


bool LMud_InputStream_Eof(struct LMud_InputStream* self)
{
    return self->data[self->index] == '\0';
}

void LMud_InputStream_Advance(struct LMud_InputStream* self)
{
    if (!LMud_InputStream_Eof(self))
        self->index++;
}

char LMud_InputStream_Get(struct LMud_InputStream* self)
{
    return self->data[self->index];
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

    saved = self->index;

    for (index = 0; str[index] != '\0'; index++)
    {
        if (!LMud_InputStream_Check(self, str[index])) {
            self->index = saved;
            return false;
        }
    }

    return true;
}
