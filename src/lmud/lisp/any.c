
#include "any.h"


#ifdef LMud_ENABLE_COMPRESSED_ANYS

LMud_AnyType LMud_Any_GetType(LMud_Any any)
{
    return (LMud_AnyType) (any & 0x03);
}

LMud_Any LMud_Any_FromPointer(void* value)
{
    return (LMud_Any) (((uintptr_t) value) | (LMud_AnyType_POINTER & 0x03));
}

LMud_Any LMud_Any_FromInteger(LMud_Integer value)
{
    return (LMud_Any) (((((uintptr_t) 0) | value) << 2) | (LMud_AnyType_INTEGER & 0x03));
}

LMud_Any LMud_Any_FromCharacter(LMud_Rune value)
{
    return (LMud_Any) (((((uintptr_t) 0) | value) << 2) | (LMud_AnyType_CHARACTER & 0x03));
}

void* LMud_Any_AsPointer(LMud_Any any)
{
    return (void*) any;
}

LMud_Integer LMud_Any_AsInteger(LMud_Any any)
{
    return (LMud_Integer) (any >> 2);
}

LMud_Rune LMud_Any_AsCharacter(LMud_Any any)
{
    return (LMud_Rune) (any >> 2);
}

bool LMud_Any_Eq(LMud_Any a, LMud_Any b)
{
    return a == b;
}

#else

LMud_AnyType LMud_Any_GetType(LMud_Any any)
{
    return any.type;
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

LMud_Any LMud_Any_FromCharacter(LMud_Rune value)
{
    LMud_Any  any;

    any.type            = LMud_AnyType_CHARACTER;
    any.value.character = value;

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

LMud_Rune LMud_Any_AsCharacter(LMud_Any any)
{
    return any.value.character;
}

bool LMud_Any_Eq(LMud_Any a, LMud_Any b)
{
    if (LMud_Any_GetType(a) != LMud_Any_GetType(b))
        return false;
    
    switch (LMud_Any_GetType(a)) {
        case LMud_AnyType_POINTER:   return LMud_Any_AsPointer(a)   == LMud_Any_AsPointer(b);
        case LMud_AnyType_INTEGER:   return LMud_Any_AsInteger(a)   == LMud_Any_AsInteger(b);
        case LMud_AnyType_CHARACTER: return LMud_Any_AsCharacter(a) == LMud_Any_AsCharacter(b);
    }

    return false;
}

#endif


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

bool LMud_Any_IsCharacter(LMud_Any any)
{
    return LMud_Any_Is(any, LMud_AnyType_CHARACTER);
}
