
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/lisp/objects.h>

struct LMud_Lisp
{
    struct LMud_Objects  objects;
};

bool LMud_Lisp_Create(struct LMud_Lisp* self);
void LMud_Lisp_Destroy(struct LMud_Lisp* self);

struct LMud_Types* LMud_Lisp_Types(struct LMud_Lisp* self);

bool LMud_Lisp_IsCons(struct LMud_Lisp* self, void* object);
bool LMud_Lisp_IsString(struct LMud_Lisp* self, void* object);
bool LMud_Lisp_IsSymbol(struct LMud_Lisp* self, void* object);

LMud_Any LMud_Lisp_Cons(struct LMud_Lisp* self, LMud_Any car, LMud_Any cdr);
LMud_Any LMud_Lisp_String(struct LMud_Lisp* self, const char* text);
LMud_Any LMud_Lisp_Intern(struct LMud_Lisp* self, const char* name);
