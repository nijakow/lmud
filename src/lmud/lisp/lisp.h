
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/lisp/objects.h>

struct LMud_Constants
{
    LMud_Any  nil;
    LMud_Any  t;
};

bool LMud_Constants_Create(struct LMud_Constants* self, struct LMud_Lisp* lisp);
void LMud_Constants_Destroy(struct LMud_Constants* self);


struct LMud_Lisp
{
    struct LMud_Objects    objects;
    struct LMud_Constants  constants;
};

bool LMud_Lisp_Create(struct LMud_Lisp* self);
void LMud_Lisp_Destroy(struct LMud_Lisp* self);

struct LMud_Types* LMud_Lisp_Types(struct LMud_Lisp* self);

bool LMud_Lisp_IsConsPointer(struct LMud_Lisp* self, void* object);
bool LMud_Lisp_IsStringPointer(struct LMud_Lisp* self, void* object);
bool LMud_Lisp_IsSymbolPointer(struct LMud_Lisp* self, void* object);

bool LMud_Lisp_IsCons(struct LMud_Lisp* self, LMud_Any value);
bool LMud_Lisp_IsString(struct LMud_Lisp* self, LMud_Any value);
bool LMud_Lisp_IsSymbol(struct LMud_Lisp* self, LMud_Any value);

bool LMud_Lisp_IsNil(struct LMud_Lisp* self, LMud_Any value);


LMud_Any LMud_Lisp_Nil(struct LMud_Lisp* self);

LMud_Any LMud_Lisp_Cons(struct LMud_Lisp* self, LMud_Any car, LMud_Any cdr);
LMud_Any LMud_Lisp_String(struct LMud_Lisp* self, const char* text);
LMud_Any LMud_Lisp_Intern(struct LMud_Lisp* self, const char* name);
LMud_Any LMud_Lisp_InternUpcase(struct LMud_Lisp* self, const char* name);


LMud_Any LMud_Lisp_Car(struct LMud_Lisp* self, LMud_Any value);
LMud_Any LMud_Lisp_Cdr(struct LMud_Lisp* self, LMud_Any value);
