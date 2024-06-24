
#include "memory.h"


void* LMud_Alloc(LMud_Size size)
{
    return malloc(size);
}

void* LMud_Realloc(void* ptr, LMud_Size size)
{
    if (ptr == NULL)
    {
        return LMud_Alloc(size);
    }

    return realloc(ptr, size);
}

void LMud_Free(void* ptr)
{
    if (ptr != NULL)
    {
        free(ptr);
    }
}


LMud_Size LMud_CStr_Length(const char* str)
{
    if (str == NULL) return 0;
    else             return strlen(str);
}

char* LMud_Strdup(const char* str)
{
    return strdup(str);
}
