
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

    uint8_t*                      bytecodes;
    LMud_Size                     bytecodes_fill;
    LMud_Size                     bytecodes_alloc;

    LMud_Any*                     constants;
    LMud_Size                     constants_fill;
    LMud_Size                     constants_alloc;
};

void LMud_Compiler_Create(struct LMud_Compiler* self, struct LMud_CompilerSession* session);
void LMud_Compiler_Destroy(struct LMud_Compiler* self);


struct LMud_Lisp* LMud_Compiler_GetLisp(struct LMud_Compiler* self);

void LMud_Compiler_PushScope(struct LMud_Compiler* self);
void LMud_Compiler_PopScope(struct LMud_Compiler* self);

void      LMud_Compiler_PushU8(struct LMud_Compiler* self, uint8_t byte);
void      LMud_Compiler_PushU16(struct LMud_Compiler* self, uint16_t word);
void      LMud_Compiler_PushBytecode(struct LMud_Compiler* self, enum LMud_Bytecode bytecode);
LMud_Size LMud_Compiler_PushConstant_None(struct LMud_Compiler* self, LMud_Any constant);
void      LMud_Compiler_PushConstant(struct LMud_Compiler* self, LMud_Any constant);

void LMud_Compiler_WriteConstant(struct LMud_Compiler* self, LMud_Any constant);
void LMud_Compiler_WritePush(struct LMud_Compiler* self);
void LMud_Compiler_WriteCall(struct LMud_Compiler* self, LMud_Size arity);

void LMud_Compiler_Compile(struct LMud_Compiler* self, LMud_Any expression);
