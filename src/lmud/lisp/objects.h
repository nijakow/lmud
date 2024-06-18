
#pragma once

#include <lmud/lisp/object.h>

struct LMud_Types
{
    struct LMud_Type     SYMBOL_TYPE;
    struct LMud_Type     STRING_TYPE;
    struct LMud_Type     CONS_TYPE;
};

void LMud_Types_Create(struct LMud_Types* self);
void LMud_Types_Destroy(struct LMud_Types* self);


struct LMud_Objects
{
    struct LMud_Object*  objects;
    struct LMud_Types    types;
};

void LMud_Objects_Create(struct LMud_Objects* self);
void LMud_Objects_Destroy(struct LMud_Objects* self);
