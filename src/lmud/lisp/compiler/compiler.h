
#pragma once

#include <lmud/lisp/base.h>

#include "session.h"


struct LMud_Scope
{
    struct LMud_Scope*  surrounding;
};

void LMud_Scope_Create(struct LMud_Scope* self, struct LMud_Scope* surrounding);
void LMud_Scope_Destroy(struct LMud_Scope* self);


struct LMud_Compiler
{
    struct LMud_CompilerSession*  session;
    struct LMud_Compiler*         lexical;
    struct LMud_Scope*            scopes;
};

void LMud_Compiler_Create(struct LMud_Compiler* self, struct LMud_CompilerSession* session);
void LMud_Compiler_Destroy(struct LMud_Compiler* self);

void LMud_Compiler_PushScope(struct LMud_Compiler* self);
void LMud_Compiler_PopScope(struct LMud_Compiler* self);

struct LMud_Lisp* LMud_Compiler_GetLisp(struct LMud_Compiler* self);

void LMud_Compiler_Compile(struct LMud_Compiler* self, LMud_Any expression);
