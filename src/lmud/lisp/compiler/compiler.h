
#pragma once

#include <lmud/lisp/base.h>

#include "session.h"


struct LMud_Register;


enum LMud_BindingType
{
    LMud_BindingType_VARIABLE,
    LMud_BindingType_FUNCTION,
};

struct LMud_Binding
{
    struct LMud_Binding*   next;
    enum LMud_BindingType  type;
    LMud_Any               name;
    struct LMud_Register*  reg;
};

void LMud_Binding_Create(struct LMud_Binding* self, enum LMud_BindingType type, LMud_Any name);
void LMud_Binding_Destroy(struct LMud_Binding* self);
void LMud_Binding_Delete(struct LMud_Binding* self);

struct LMud_Register* LMud_Binding_GetRegister(struct LMud_Binding* self);
void                  LMud_Binding_SetRegister(struct LMud_Binding* self, struct LMud_Register* reg);


struct LMud_Scope
{
    struct LMud_Scope*    surrounding;
    struct LMud_Binding*  bindings;
};

void LMud_Scope_Create(struct LMud_Scope* self, struct LMud_Scope* surrounding);
void LMud_Scope_Destroy(struct LMud_Scope* self);

struct LMud_Binding* LMud_Scope_FindBinding(struct LMud_Scope* self, LMud_Any name, enum LMud_BindingType type);
struct LMud_Binding* LMud_Scope_CreateBinding(struct LMud_Scope* self, LMud_Any name, enum LMud_BindingType type);


#define LMud_CompilerLabelInfo_NOT_PLACED  ((LMud_Size) -1)

struct LMud_CompilerLabelInfo
{
    struct LMud_CompilerLabelInfo**  prev;
    struct LMud_CompilerLabelInfo*   next;
    LMud_Size                        offset;
    LMud_Size*                       targets;
    LMud_Size                        targets_fill;
    LMud_Size                        targets_alloc;
};

void LMud_CompilerLabelInfo_Create(struct LMud_CompilerLabelInfo* self, struct LMud_CompilerLabelInfo** list);
void LMud_CompilerLabelInfo_Destroy(struct LMud_CompilerLabelInfo* self);
void LMud_CompilerLabelInfo_Delete(struct LMud_CompilerLabelInfo* self);

bool LMud_CompilerLabelInfo_IsPlaced(struct LMud_CompilerLabelInfo* self);
bool LMud_CompilerLabelInfo_Place(struct LMud_CompilerLabelInfo* self, LMud_Size offset);

LMud_Size LMud_CompilerLabelInfo_GetOffset(struct LMud_CompilerLabelInfo* self);

void LMud_CompilerLabelInfo_AddTarget(struct LMud_CompilerLabelInfo* self, LMud_Size target);
bool LMud_CompilerLabelInfo_WriteTargets(struct LMud_CompilerLabelInfo* self, uint8_t* bytecodes);



struct LMud_Register
{
    struct LMud_Register**  prev;
    struct LMud_Register*   next;
    LMud_Size               index;
};

void LMud_Register_Create(struct LMud_Register* self, LMud_Size index, struct LMud_Register** list);
void LMud_Register_Destroy(struct LMud_Register* self);
void LMud_Register_Delete(struct LMud_Register* self);


struct LMud_Compiler
{
    struct LMud_CompilerSession*    session;
    struct LMud_Compiler*           lexical;
    struct LMud_Scope*              scopes;

    uint8_t*                        bytecodes;
    LMud_Size                       bytecodes_fill;
    LMud_Size                       bytecodes_alloc;

    LMud_Any*                       constants;
    LMud_Size                       constants_fill;
    LMud_Size                       constants_alloc;

    struct LMud_CompilerLabelInfo*  labels;
    struct LMud_Register*           registers;

    struct
    {
        LMud_Any                    symbol_symbol_value;
        LMud_Any                    symbol_set_symbol_value;
        LMud_Any                    symbol_set_symbol_function;

        LMud_Any                    symbol_quote;
        LMud_Any                    symbol_function;
        LMud_Any                    symbol_lambda;
        LMud_Any                    symbol_progn;
        LMud_Any                    symbol_setq;
        LMud_Any                    symbol_let;
        LMud_Any                    symbol_flet;
        LMud_Any                    symbol_labels;
        LMud_Any                    symbol_if;
    }                               cached;
};

typedef struct LMud_CompilerLabelInfo* LMud_CompilerLabel;

void LMud_Compiler_Create(struct LMud_Compiler* self, struct LMud_CompilerSession* session);
void LMud_Compiler_Create_Lexical(struct LMud_Compiler* self, struct LMud_Compiler* lexical);
void LMud_Compiler_Destroy(struct LMud_Compiler* self);


struct LMud_Lisp* LMud_Compiler_GetLisp(struct LMud_Compiler* self);

void LMud_Compiler_PushScope(struct LMud_Compiler* self);
void LMud_Compiler_PopScope(struct LMud_Compiler* self);

struct LMud_Register* LMud_Compiler_AllocateRegister(struct LMud_Compiler* self);

void      LMud_Compiler_PushU8(struct LMud_Compiler* self, uint8_t byte);
void      LMud_Compiler_PushU16(struct LMud_Compiler* self, uint16_t word);
void      LMud_Compiler_PushBytecode(struct LMud_Compiler* self, enum LMud_Bytecode bytecode);
LMud_Size LMud_Compiler_PushConstant_None(struct LMud_Compiler* self, LMud_Any constant);
void      LMud_Compiler_PushConstant(struct LMud_Compiler* self, LMud_Any constant);

bool LMud_Compiler_OpenLabel(struct LMud_Compiler* self, LMud_CompilerLabel* label);
void LMud_Compiler_CloseLabel(struct LMud_Compiler* self, LMud_CompilerLabel label);
void LMud_Compiler_PlaceLabel(struct LMud_Compiler* self, LMud_CompilerLabel label);
void LMud_Compiler_WriteJump(struct LMud_Compiler* self, LMud_CompilerLabel label);
void LMud_Compiler_WriteJumpIfNil(struct LMud_Compiler* self, LMud_CompilerLabel label);

void LMud_Compiler_WriteConstant(struct LMud_Compiler* self, LMud_Any constant);
void LMud_Compiler_WriteLambda(struct LMud_Compiler* self, LMud_Any lambda);
void LMud_Compiler_WriteSymbolVariable(struct LMud_Compiler* self, LMud_Any symbol);
void LMud_Compiler_WriteSymbolFunction(struct LMud_Compiler* self, LMud_Any symbol);
void LMud_Compiler_WriteLoad(struct LMud_Compiler* self, LMud_Size depth, LMud_Size index);
void LMud_Compiler_WriteStore(struct LMud_Compiler* self, LMud_Size depth, LMud_Size index);
void LMud_Compiler_WritePush(struct LMud_Compiler* self);
void LMud_Compiler_WriteCall(struct LMud_Compiler* self, LMud_Size arity);

void LMud_Compiler_Compile(struct LMud_Compiler* self, LMud_Any expression);
void LMud_Compiler_CompileExpressions(struct LMud_Compiler* self, LMud_Any expressions);

LMud_Any LMud_Compiler_Build(struct LMud_Compiler* self);
