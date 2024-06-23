
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

    self->bytecodes       = NULL;
    self->bytecodes_fill  = 0;
    self->bytecodes_alloc = 0;

    self->constants       = NULL;
    self->constants_fill  = 0;
    self->constants_alloc = 0;

    LMud_Compiler_PushScope(self);
}

void LMud_Compiler_Destroy(struct LMud_Compiler* self)
{
    while (self->scopes != NULL)
    {
        LMud_Compiler_PopScope(self);
    }

    LMud_Free(self->bytecodes);
    LMud_Free(self->constants);
}


struct LMud_Lisp* LMud_Compiler_GetLisp(struct LMud_Compiler* self)
{
    return LMud_CompilerSession_GetLisp(self->session);
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


void LMud_Compiler_PushU8(struct LMud_Compiler* self, uint8_t byte)
{
    LMud_Size  new_size;
    uint8_t*   new_bytecodes;

    if (self->bytecodes_fill >= self->bytecodes_alloc)
    {
        if (self->bytecodes_alloc == 0) {
            new_size = 1;
        } else {
            new_size = self->bytecodes_alloc * 2;
        }

        new_bytecodes = LMud_Realloc(self->bytecodes, new_size);

        // TODO: Error if new_bytecodes == NULL

        self->bytecodes       = new_bytecodes;
        self->bytecodes_alloc = new_size;
    }

    self->bytecodes[self->bytecodes_fill++] = byte;
}

void LMud_Compiler_PushU16(struct LMud_Compiler* self, uint16_t word)
{
    LMud_Compiler_PushU8(self, (word >> 8) & 0xFF);
    LMud_Compiler_PushU8(self, word & 0xFF);
}

void LMud_Compiler_PushBytecode(struct LMud_Compiler* self, enum LMud_Bytecode bytecode)
{
    LMud_Compiler_PushU8(self, (uint8_t) bytecode);
}

LMud_Size LMud_Compiler_PushConstant_None(struct LMud_Compiler* self, LMud_Any constant)
{
    LMud_Size  index;
    LMud_Size  new_size;
    LMud_Any*  new_constants;

    if (self->constants_fill >= self->constants_alloc)
    {
        if (self->constants_alloc == 0) {
            new_size = 1;
        } else {
            new_size = self->constants_alloc * 2;
        }

        new_constants = LMud_Realloc(self->constants, new_size * sizeof(LMud_Any));

        // TODO: Error if new_constants == NULL

        self->constants       = new_constants;
        self->constants_alloc = new_size;
    }

    index = self->constants_fill++;

    self->constants[index] = constant;

    return index;
}

void LMud_Compiler_PushConstant(struct LMud_Compiler* self, LMud_Any constant)
{
    LMud_Compiler_PushU16(self, LMud_Compiler_PushConstant_None(self, constant));
}


void LMud_Compiler_WriteConstant(struct LMud_Compiler* self, LMud_Any constant)
{
    LMud_Compiler_PushBytecode(self, LMud_Bytecode_CONSTANT);
    LMud_Compiler_PushConstant(self, constant);
}

void LMud_Compiler_WritePush(struct LMud_Compiler* self)
{
    LMud_Compiler_PushBytecode(self, LMud_Bytecode_PUSH);
}

void LMud_Compiler_WriteCall(struct LMud_Compiler* self, LMud_Size arity)
{
    LMud_Compiler_PushBytecode(self, LMud_Bytecode_CALL);
    LMud_Compiler_PushU8(self, arity);
}


void LMud_Compiler_CompileConstant(struct LMud_Compiler* self, LMud_Any expression)
{
    LMud_Compiler_WriteConstant(self, expression);
}

void LMud_Compiler_CompileVariable(struct LMud_Compiler* self, LMud_Any expression)
{
    (void) self;
    (void) expression;
}

void LMud_Compiler_CompileFunction(struct LMud_Compiler* self, LMud_Any expression)
{
    (void) self;
    (void) expression;
}

void LMud_Compiler_CompileFuncall(struct LMud_Compiler* self, LMud_Any function, LMud_Any arguments)
{
    LMud_Any   argument;
    LMud_Size  arity;

    arity = 0;

    while (!LMud_Lisp_IsNil(LMud_Compiler_GetLisp(self), arguments))
    {
        argument = LMud_Lisp_Car(LMud_Compiler_GetLisp(self), arguments);

        LMud_Compiler_Compile(self, argument);
        LMud_Compiler_WritePush(self);

        arguments = LMud_Lisp_Cdr(LMud_Compiler_GetLisp(self), arguments);
        arity     = arity + 1;
    }

    LMud_Compiler_CompileFunction(self, function);
    LMud_Compiler_WriteCall(self, arity);
}

void LMud_Compiler_CompileCombination(struct LMud_Compiler* self, LMud_Any expression)
{
    LMud_Any  function;
    LMud_Any  arguments;

    function  = LMud_Lisp_Car(LMud_Compiler_GetLisp(self), expression);
    arguments = LMud_Lisp_Cdr(LMud_Compiler_GetLisp(self), expression);

    /*
     * TODO: Handle special forms
     */

    LMud_Compiler_CompileFuncall(self, function, arguments);
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
