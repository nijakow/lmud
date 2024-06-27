
#pragma once

#include <lmud/defs.h>
#include <lmud/util/utf8.h>


typedef enum LMud_AnyType
{
    LMud_AnyType_POINTER,
    LMud_AnyType_INTEGER,
    LMud_AnyType_CHARACTER,
} LMud_AnyType;

typedef union LMud_AnyValue
{
    void*         pointer;
    LMud_Integer  integer;
    LMud_Rune     character;
} LMud_AnyValue;

typedef struct LMud_Any
{
    LMud_AnyValue  value;
    LMud_AnyType   type;
} LMud_Any;


LMud_AnyType LMud_Any_GetType(LMud_Any any);

bool LMud_Any_Is(LMud_Any any, LMud_AnyType type);
bool LMud_Any_IsPointer(LMud_Any any);
bool LMud_Any_IsInteger(LMud_Any any);
bool LMud_Any_IsCharacter(LMud_Any any);

LMud_Any LMud_Any_FromPointer(void* value);
LMud_Any LMud_Any_FromInteger(LMud_Integer value);
LMud_Any LMud_Any_FromCharacter(LMud_Rune value);

void*         LMud_Any_AsPointer(LMud_Any any);
LMud_Integer  LMud_Any_AsInteger(LMud_Any any);
LMud_Rune     LMud_Any_AsCharacter(LMud_Any any);

bool LMud_Any_Eq(LMud_Any a, LMud_Any b);
