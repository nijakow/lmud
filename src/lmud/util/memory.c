
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
