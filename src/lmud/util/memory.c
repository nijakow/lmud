
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

void LMud_TrimMalloc()
{
#ifdef LMud_ENABLE_MALLOC_TRIM
    malloc_trim(0);
#endif
}


LMud_Size LMud_CStr_Length(const char* str)
{
    if (str == NULL) return 0;
    else             return strlen(str);
}

bool LMud_CStr_Equals(const char* a, const char* b)
{
    if (a == NULL && b == NULL) return true;
    if (a == NULL || b == NULL) return false;

    return strcmp(a, b) == 0;
}

bool LMud_CStr_EqualsIgnoreCase(const char* a, const char* b)
{
    if (a == NULL && b == NULL) return true;
    if (a == NULL || b == NULL) return false;

    return strcasecmp(a, b) == 0;
}

bool LMud_CStr_FindAndPartition(const char* cstr, const char* separator, const char** end, const char** after)
{
    const char*  found;

    found = strstr(cstr, separator);

    if (found == NULL)
    {
        return false;
    }

    *end   = found;
    *after = found + strlen(separator);

    return true;
}

char* LMud_Strdup(const char* str)
{
    return strdup(str);
}

void LMud_CopyMemory(void* dest, const void* src, LMud_Size size)
{
    memcpy(dest, src, size);
}
