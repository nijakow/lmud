
#include <lmud/lisp/lisp.h>
#include <lmud/util/memory.h>

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

    LMud_Compiler_PushScope(self);
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
    struct LMud_Scope*  scope;

    scope = LMud_Alloc(sizeof(struct LMud_Scope));

    // TODO: Error if scope == NULL

    LMud_Scope_Create(scope, self->scopes);

    self->scopes = scope;
}

void LMud_Compiler_PopScope(struct LMud_Compiler* self)
{
    struct LMud_Scope*  scope;

    scope        = self->scopes;
    self->scopes = scope->surrounding;

    LMud_Scope_Destroy(scope);
    LMud_Free(scope);
}



struct LMud_Lisp* LMud_Compiler_GetLisp(struct LMud_Compiler* self)
{
    return LMud_CompilerSession_GetLisp(self->session);
}


void LMud_Compiler_CompileConstant(struct LMud_Compiler* self, LMud_Any expression)
{
    (void) self;
    (void) expression;
}

void LMud_Compiler_CompileVariable(struct LMud_Compiler* self, LMud_Any expression)
{
    (void) self;
    (void) expression;
}

void LMud_Compiler_CompileCombination(struct LMud_Compiler* self, LMud_Any expression)
{
    (void) self;
    (void) expression;
}

void LMud_Compiler_Compile(struct LMud_Compiler* self, LMud_Any expression)
{
    if (LMud_Lisp_IsSymbol(LMud_Compiler_GetLisp(self), expression))
        LMud_Compiler_CompileVariable(self, expression);
    else if (LMud_Lisp_IsCons(LMud_Compiler_GetLisp(self), expression))
        LMud_Compiler_CompileCombination(self, expression);
    else
        LMud_Compiler_CompileConstant(self, expression);
}
