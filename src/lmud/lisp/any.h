
#pragma once

#include <lmud/defs.h>


typedef enum LMud_AnyType
{
    LMud_AnyType_POINTER,
    LMud_AnyType_INTEGER,
} LMud_AnyType;

typedef union LMud_AnyValue
{
    struct LMud_Object*  pointer;
    LMud_Integer         integer;
} LMud_AnyValue;

typedef struct LMud_Any
{
    LMud_AnyValue  value;
    LMud_AnyType   type;
} LMud_Any;


LMud_Any LMud_Any_FromPointer(struct LMud_Object* value);
LMud_Any LMud_Any_FromInteger(LMud_Integer value);

LMud_AnyType LMud_Any_GetType(LMud_Any any);

bool LMud_Any_Is(LMud_Any any, LMud_AnyType type);
bool LMud_Any_IsPointer(LMud_Any any);
bool LMud_Any_IsInteger(LMud_Any any);
