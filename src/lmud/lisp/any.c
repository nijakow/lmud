
#include "any.h"


LMud_Any LMud_Any_FromPointer(struct LMud_Object* value)
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
