
#include "lisp.h"


bool LMud_Lisp_Create(struct LMud_Lisp* self)
{
    return LMud_Objects_Create(&self->objects);
}

void LMud_Lisp_Destroy(struct LMud_Lisp* self)
{
    LMud_Objects_Destroy(&self->objects);
}


struct LMud_Types* LMud_Lisp_Types(struct LMud_Lisp* self)
{
    return &self->objects.types;
}


bool LMud_Lisp_IsCons(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsCons(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsString(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsString(LMud_Lisp_Types(self), object);
}

bool LMud_Lisp_IsSymbol(struct LMud_Lisp* self, void* object)
{
    return LMud_Types_IsSymbol(LMud_Lisp_Types(self), object);
}


LMud_Any LMud_Lisp_Cons(struct LMud_Lisp* self, LMud_Any car, LMud_Any cdr)
{
    return LMud_Any_FromPointer(LMud_Objects_Cons(&self->objects, car, cdr));
}

LMud_Any LMud_Lisp_String(struct LMud_Lisp* self, const char* text)
{
    return LMud_Any_FromPointer(LMud_Objects_String(&self->objects, text));
}

LMud_Any LMud_Lisp_Intern(struct LMud_Lisp* self, const char* name)
{
    return LMud_Any_FromPointer(LMud_Objects_Intern(&self->objects, name));
}
