
#include "session.h"

void LMud_CompilerSession_Create(struct LMud_CompilerSession* self, struct LMud_Lisp* lisp)
{
    self->lisp = lisp;
}

void LMud_CompilerSession_Destroy(struct LMud_CompilerSession* self)
{
    (void) self;
}


struct LMud_Lisp* LMud_CompilerSession_GetLisp(struct LMud_CompilerSession* self)
{
    return self->lisp;
}
