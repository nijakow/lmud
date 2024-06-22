
#include "compiler.h"


void LMud_Scope_Create(struct LMud_Scope* self, struct LMud_Scope* surrounding)
{
    self->surrounding = surrounding;
}

void LMud_Scope_Destroy(struct LMud_Scope* self)
{
    (void) self;
}


void LMud_Compiler_Create(struct LMud_Compiler* self, struct LMud_CompilerSession* session)
{
    self->session = session;
    self->lexical = NULL;
    self->scopes  = NULL;
}

void LMud_Compiler_Destroy(struct LMud_Compiler* self)
{
    while (self->scopes != NULL)
    {
        LMud_Compiler_PopScope(self);
    }
}


void LMud_Compiler_PushScope(struct LMud_Compiler* self)
{
    // TODO
    (void) self;
}

void LMud_Compiler_PopScope(struct LMud_Compiler* self)
{
    // TODO
    (void) self;
}
