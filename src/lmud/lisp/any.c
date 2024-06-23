
#include "any.h"


LMud_AnyType LMud_Any_GetType(LMud_Any any)
{
    return any.type;
}

bool LMud_Any_Is(LMud_Any any, LMud_AnyType type)
{
    return LMud_Any_GetType(any) == type;
}

bool LMud_Any_IsPointer(LMud_Any any)
{
    return LMud_Any_Is(any, LMud_AnyType_POINTER);
}

bool LMud_Any_IsInteger(LMud_Any any)
{
    return LMud_Any_Is(any, LMud_AnyType_INTEGER);
}


LMud_Any LMud_Any_FromPointer(void* value)
{
    LMud_Any  any;

    any.type          = LMud_AnyType_POINTER;
    any.value.pointer = value;

    return any;
}

LMud_Any LMud_Any_FromInteger(LMud_Integer value)
{
    LMud_Any  any;

    any.type          = LMud_AnyType_INTEGER;
    any.value.integer = value;

    return any;
}


void* LMud_Any_AsPointer(LMud_Any any)
{
    return any.value.pointer;
}

LMud_Integer LMud_Any_AsInteger(LMud_Any any)
{
    return any.value.integer;
}

bool LMud_Any_Eq(LMud_Any a, LMud_Any b)
{
    if (LMud_Any_GetType(a) != LMud_Any_GetType(b))
        return false;
    
    switch (LMud_Any_GetType(a)) {
        case LMud_AnyType_POINTER: return LMud_Any_AsPointer(a) == LMud_Any_AsPointer(b);
        case LMud_AnyType_INTEGER: return LMud_Any_AsInteger(a) == LMud_Any_AsInteger(b);
    }

    return false;
}
