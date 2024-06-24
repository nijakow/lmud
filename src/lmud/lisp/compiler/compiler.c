
#include <lmud/lisp/lisp.h>
#include <lmud/util/memory.h>

#include "compiler.h"


void LMud_Binding_Create(struct LMud_Binding* self, enum LMud_BindingType type, LMud_Any name)
{
    self->next = NULL;
    self->type = type;
    self->name = name;
    self->reg  = NULL;
}

void LMud_Binding_Destroy(struct LMud_Binding* self)
{
    (void) self;
}

void LMud_Binding_Delete(struct LMud_Binding* self)
{
    LMud_Binding_Destroy(self);
    LMud_Free(self);
}


struct LMud_Register* LMud_Binding_GetRegister(struct LMud_Binding* self)
{
    return self->reg;
}

void LMud_Binding_SetRegister(struct LMud_Binding* self, struct LMud_Register* reg)
{
    self->reg = reg;
}



void LMud_Scope_Create(struct LMud_Scope* self, struct LMud_Scope* surrounding)
{
    self->surrounding = surrounding;
    self->bindings    = NULL;
}

void LMud_Scope_Destroy(struct LMud_Scope* self)
{
    struct LMud_Binding*  binding;

    while (self->bindings != NULL)
    {
        binding        = self->bindings;
        self->bindings = binding->next;

        LMud_Binding_Delete(binding);
    }
}

struct LMud_Binding* LMud_Scope_FindBinding(struct LMud_Scope* self, LMud_Any name, enum LMud_BindingType type)
{
    struct LMud_Binding*  binding;

    for (binding = self->bindings; binding != NULL; binding = binding->next)
    {
        if (LMud_Any_Eq(binding->name, name) && binding->type == type)
            return binding;
    }

    return NULL;
}

struct LMud_Binding* LMud_Scope_CreateBinding(struct LMud_Scope* self, LMud_Any name, enum LMud_BindingType type)
{
    struct LMud_Binding*  binding;

    binding = LMud_Alloc(sizeof(struct LMud_Binding));

    if (binding != NULL)
    {
        LMud_Binding_Create(binding, type, name);

        binding->next  = self->bindings;
        self->bindings = binding;
    }

    return binding;
}


void LMud_CompilerLabelInfo_Create(struct LMud_CompilerLabelInfo* self, struct LMud_CompilerLabelInfo** list)
{
    /*
     * Link ourself into the list.
     */
    if (*list != NULL)
        (*list)->prev = &self->next;
    self->prev =  list;
    self->next = *list;
    *list      =  self;

    /*
     * Set the placement status to "not placed".
     */
    self->offset = LMud_CompilerLabelInfo_NOT_PLACED;

    /*
     * Initialize the target list.
     */
    self->targets       = NULL;
    self->targets_fill  = 0;
    self->targets_alloc = 0;
}

void LMud_CompilerLabelInfo_Destroy(struct LMud_CompilerLabelInfo* self)
{
    /*
     * Unlink ourself from the list.
     */
    if (self->next != NULL)
        self->next->prev = self->prev;
    *self->prev = self->next;

    /*
     * Free the target list.
     */
    LMud_Free(self->targets);
}

void LMud_CompilerLabelInfo_Delete(struct LMud_CompilerLabelInfo* self)
{
    LMud_CompilerLabelInfo_Destroy(self);
    LMud_Free(self);
}

bool LMud_CompilerLabelInfo_IsPlaced(struct LMud_CompilerLabelInfo* self)
{
    return self->offset != LMud_CompilerLabelInfo_NOT_PLACED;
}

bool LMud_CompilerLabelInfo_Place(struct LMud_CompilerLabelInfo* self, LMud_Size offset)
{
    if (self->offset != LMud_CompilerLabelInfo_NOT_PLACED)
        return false;

    self->offset = offset;

    return true;
}

LMud_Size LMud_CompilerLabelInfo_GetOffset(struct LMud_CompilerLabelInfo* self)
{
    return self->offset;
}

void LMud_CompilerLabelInfo_AddTarget(struct LMud_CompilerLabelInfo* self, LMud_Size target)
{
    LMud_Size  new_size;
    LMud_Size* new_targets;

    if (self->targets_fill >= self->targets_alloc)
    {
        if (self->targets_alloc == 0) {
            new_size = 1;
        } else {
            new_size = self->targets_alloc * 2;
        }

        new_targets = LMud_Realloc(self->targets, new_size * sizeof(LMud_Size));

        // TODO: Error if new_targets == NULL

        self->targets       = new_targets;
        self->targets_alloc = new_size;
    }

    self->targets[self->targets_fill++] = target;
}

bool LMud_CompilerLabelInfo_WriteTargets(struct LMud_CompilerLabelInfo* self, uint8_t* bytecodes)
{
    LMud_Size  index;

    if (!LMud_CompilerLabelInfo_IsPlaced(self))
        return false;

    for (index = 0; index < self->targets_fill; index++)
    {
        bytecodes[self->targets[index] + 0] = (self->offset >> 8) & 0xFF;
        bytecodes[self->targets[index] + 1] = self->offset & 0xFF;
    }

    self->targets_fill = 0;

    return true;
}


void LMud_Register_Create(struct LMud_Register* self, LMud_Size index, struct LMud_Register** list)
{
    self->index =  index;

    self->prev  =  list;
    self->next  = *list;
    *list       =  self;
}

void LMud_Register_Destroy(struct LMud_Register* self)
{
    /*
     * Unlink ourself from the list.
     */
    *self->prev = self->next;
    if (self->next != NULL)
        self->next->prev = self->prev;
}

void LMud_Register_Delete(struct LMud_Register* self)
{
    LMud_Register_Destroy(self);
    LMud_Free(self);
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

    self->labels          = NULL;
    self->registers       = NULL;

    self->max_stack_depth     = 0;
    self->current_stack_depth = 0;
    self->max_register_index  = 0;
    self->uses_lexical_stuff  = false;

    self->cached.symbol_quote    = LMud_Lisp_Intern(LMud_CompilerSession_GetLisp(session), "QUOTE");
    self->cached.symbol_function = LMud_Lisp_Intern(LMud_CompilerSession_GetLisp(session), "FUNCTION");
    self->cached.symbol_lambda   = LMud_Lisp_Intern(LMud_CompilerSession_GetLisp(session), "LAMBDA");
    self->cached.symbol_progn    = LMud_Lisp_Intern(LMud_CompilerSession_GetLisp(session), "PROGN");
    self->cached.symbol_setq     = LMud_Lisp_Intern(LMud_CompilerSession_GetLisp(session), "SETQ");
    self->cached.symbol_let      = LMud_Lisp_Intern(LMud_CompilerSession_GetLisp(session), "LET");
    self->cached.symbol_flet     = LMud_Lisp_Intern(LMud_CompilerSession_GetLisp(session), "FLET");
    self->cached.symbol_labels   = LMud_Lisp_Intern(LMud_CompilerSession_GetLisp(session), "LABELS");
    self->cached.symbol_if       = LMud_Lisp_Intern(LMud_CompilerSession_GetLisp(session), "IF");

    LMud_Compiler_PushScope(self);
}

void LMud_Compiler_Create_Lexical(struct LMud_Compiler* self, struct LMud_Compiler* lexical)
{
    LMud_Compiler_Create(self, lexical->session);
    self->lexical = lexical;
}

void LMud_Compiler_Destroy(struct LMud_Compiler* self)
{
    while (self->scopes != NULL)
    {
        LMud_Compiler_PopScope(self);
    }

    while (self->labels != NULL)
    {
        LMud_CompilerLabelInfo_Delete(self->labels);
    }

    while (self->registers != NULL)
    {
        LMud_Register_Delete(self->registers);
    }

    LMud_Free(self->bytecodes);
    LMud_Free(self->constants);
}


struct LMud_Lisp* LMud_Compiler_GetLisp(struct LMud_Compiler* self)
{
    return LMud_CompilerSession_GetLisp(self->session);
}


void LMud_Compiler_IncreaseStackDepth(struct LMud_Compiler* self, LMud_Size depth)
{
    self->current_stack_depth += depth;
    if (self->current_stack_depth > self->max_stack_depth)
        self->max_stack_depth = self->current_stack_depth;
}

void LMud_Compiler_DecreaseStackDepth(struct LMud_Compiler* self, LMud_Size depth)
{
    self->current_stack_depth -= depth;
}

void LMud_Compiler_EnableLexicalStuff(struct LMud_Compiler* self)
{
    self->uses_lexical_stuff = true;
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


bool LMud_Compiler_FindBinding(struct LMud_Compiler* self, LMud_Any name, enum LMud_BindingType type, struct LMud_Binding** binding)
{
    struct LMud_Compiler*  compiler;
    struct LMud_Scope*     scope;

    for (compiler = self; compiler != NULL; compiler = compiler->lexical)
    {
        for (scope = compiler->scopes; scope != NULL; scope = scope->surrounding)
        {
            *binding = LMud_Scope_FindBinding(scope, name, type);

            if (*binding != NULL)
                return true;
        }
    }

    return false;
}

void LMud_Compiler_BindRegister(struct LMud_Compiler* self, LMud_Any name, enum LMud_BindingType type, struct LMud_Register* reg)
{
    struct LMud_Binding*  binding;

    if (!LMud_Compiler_FindBinding(self, name, type, &binding))
    {
        binding = LMud_Scope_CreateBinding(self->scopes, name, type);
    }

    LMud_Binding_SetRegister(binding, reg);
}


struct LMud_Register* LMud_Compiler_AllocateRegister(struct LMud_Compiler* self)
{
    struct LMud_Register*  reg;
    LMud_Size              index;

    reg = LMud_Alloc(sizeof(struct LMud_Register));

    if (reg != NULL)
    {
        index = (self->registers == NULL) ? 0 : (self->registers->index + 1);

        if ((index + 1) > self->max_register_index)
            self->max_register_index = (index + 1);

        LMud_Register_Create(reg, index, &self->registers);
    }

    return reg;
}

bool LMud_Compiler_IdentifyRegister(struct LMud_Compiler* self, struct LMud_Register* reg, LMud_Size* depth, LMud_Size* index)
{
    struct LMud_Compiler*  compiler;
    struct LMud_Register*  the_reg;
    LMud_Size              our_depth;

    for (compiler = self, our_depth = 0; compiler != NULL; compiler = compiler->lexical, our_depth++)
    {
        for (the_reg = compiler->registers; the_reg != NULL; the_reg = the_reg->next)
        {
            if (the_reg == reg)
            {
                *depth = our_depth;
                *index = the_reg->index;
                return true;
            }
        }
    }

    return false;
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

    for (index = 0; index < self->constants_fill; index++)
    {
        if (LMud_Any_Eq(self->constants[index], constant))
            return index;
    }

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

bool LMud_Compiler_OpenLabel(struct LMud_Compiler* self, LMud_CompilerLabel* label)
{
    struct LMud_CompilerLabelInfo*  the_label;

    the_label = LMud_Alloc(sizeof(struct LMud_CompilerLabelInfo));

    // TODO: Error if label == NULL

    LMud_CompilerLabelInfo_Create(the_label, &self->labels);

    *label = the_label;

    return true;
}

void LMud_Compiler_CloseLabel(struct LMud_Compiler* self, LMud_CompilerLabel label)
{
    /*
     * TODO: Warning / Error if the label has not been placed or used.
     */
    (void) self;
    LMud_CompilerLabelInfo_Delete(label);
}

void LMud_Compiler_PlaceLabel(struct LMud_Compiler* self, LMud_CompilerLabel label)
{
    /*
     * TODO: Error if label is already placed.
     */
    LMud_CompilerLabelInfo_Place(label, self->bytecodes_fill);
    LMud_CompilerLabelInfo_WriteTargets(label, self->bytecodes);
}

void LMud_Compiler_WriteLabel(struct LMud_Compiler* self, LMud_CompilerLabel label)
{
    if (LMud_CompilerLabelInfo_IsPlaced(label))
        LMud_Compiler_PushU16(self, LMud_CompilerLabelInfo_GetOffset((struct LMud_CompilerLabelInfo*) label));
    else {
        LMud_CompilerLabelInfo_AddTarget(label, self->bytecodes_fill);
        LMud_Compiler_PushU16(self, 0xffff);
    }
}

void LMud_Compiler_WriteJump(struct LMud_Compiler* self, LMud_CompilerLabel label)
{
    LMud_Compiler_PushBytecode(self, LMud_Bytecode_JUMP);
    LMud_Compiler_WriteLabel(self, label);
}

void LMud_Compiler_WriteJumpIfNil(struct LMud_Compiler* self, LMud_CompilerLabel label)
{
    LMud_Compiler_PushBytecode(self, LMud_Bytecode_JUMP_IF_NIL);
    LMud_Compiler_WriteLabel(self, label);
}


void LMud_Compiler_WriteConstant(struct LMud_Compiler* self, LMud_Any constant)
{
    LMud_Compiler_PushBytecode(self, LMud_Bytecode_CONSTANT);
    LMud_Compiler_PushConstant(self, constant);
}

void LMud_Compiler_WriteLambda(struct LMud_Compiler* self, LMud_Any lambda)
{
    LMud_Compiler_PushBytecode(self, LMud_Bytecode_LAMBDA);
    LMud_Compiler_PushConstant(self, lambda);
}


void LMud_Compiler_WriteSymbolVariable(struct LMud_Compiler* self, LMud_Any symbol)
{
    LMud_Compiler_PushBytecode(self, LMud_Bytecode_SYMBOL_VARIABLE);
    LMud_Compiler_PushConstant(self, symbol);
}

void LMud_Compiler_WriteSymbolFunction(struct LMud_Compiler* self, LMud_Any symbol)
{
    LMud_Compiler_PushBytecode(self, LMud_Bytecode_SYMBOL_FUNCTION);
    LMud_Compiler_PushConstant(self, symbol);
}

void LMud_Compiler_WriteLoad(struct LMud_Compiler* self, LMud_Size depth, LMud_Size index)
{
    if (depth > 0)
        LMud_Compiler_EnableLexicalStuff(self);
    LMud_Compiler_PushBytecode(self, LMud_Bytecode_LEXICAL_LOAD);
    LMud_Compiler_PushU8(self, depth);
    LMud_Compiler_PushU8(self, index);
}

void LMud_Compiler_WriteStore(struct LMud_Compiler* self, LMud_Size depth, LMud_Size index)
{
    if (depth > 0)
        LMud_Compiler_EnableLexicalStuff(self);
    LMud_Compiler_PushBytecode(self, LMud_Bytecode_LEXICAL_STORE);
    LMud_Compiler_PushU8(self, depth);
    LMud_Compiler_PushU8(self, index);
}

void LMud_Compiler_WritePush(struct LMud_Compiler* self)
{
    LMud_Compiler_PushBytecode(self, LMud_Bytecode_PUSH);
    LMud_Compiler_IncreaseStackDepth(self, 1);
}

void LMud_Compiler_WriteCall(struct LMud_Compiler* self, LMud_Size arity)
{
    LMud_Compiler_PushBytecode(self, LMud_Bytecode_CALL);
    LMud_Compiler_PushU8(self, arity);
    LMud_Compiler_DecreaseStackDepth(self, arity);
}

void LMud_Compiler_WriteReturn(struct LMud_Compiler* self)
{
    LMud_Compiler_PushBytecode(self, LMud_Bytecode_RETURN);
}



bool LMud_Compiler_WriteLoadRegister(struct LMud_Compiler* self, struct LMud_Register* reg)
{
    LMud_Size  depth;
    LMud_Size  index;

    if (LMud_Compiler_IdentifyRegister(self, reg, &depth, &index)) {
        LMud_Compiler_WriteLoad(self, depth, index);
        return true;
    } else {
        return false;
    }
}

bool LMud_Compiler_WriteStoreRegister(struct LMud_Compiler* self, struct LMud_Register* reg)
{
    LMud_Size  depth;
    LMud_Size  index;

    if (LMud_Compiler_IdentifyRegister(self, reg, &depth, &index)) {
        LMud_Compiler_WriteStore(self, depth, index);
        return true;
    } else {
        return false;
    }
}


void LMud_Compiler_CompileConstant(struct LMud_Compiler* self, LMud_Any expression)
{
    LMud_Compiler_WriteConstant(self, expression);
}

enum LMud_VariableMode
{
    LMud_VariableMode_LOAD,
    LMud_VariableMode_STORE
};

void LMud_Compiler_CompileVariable(struct LMud_Compiler* self, LMud_Any expression, enum LMud_BindingType type, enum LMud_VariableMode mode)
{
    struct LMud_Binding*  binding;

    if (LMud_Compiler_FindBinding(self, expression, type, &binding))
    {
        if (LMud_Binding_GetRegister(binding) != NULL) {
            switch (mode)
            {
                case LMud_VariableMode_LOAD:
                    LMud_Compiler_WriteLoadRegister(self, LMud_Binding_GetRegister(binding));
                    break;

                case LMud_VariableMode_STORE:
                    LMud_Compiler_WriteStoreRegister(self, LMud_Binding_GetRegister(binding));
                    break;
                
                // TODO: Error on default
            }
            return;
        }
    }

    switch (type)
    {
        case LMud_BindingType_VARIABLE: // TODO: Load vs. Store
            LMud_Compiler_WriteSymbolVariable(self, expression);
            break;

        case LMud_BindingType_FUNCTION: // TODO: Load vs. Store
            LMud_Compiler_WriteSymbolFunction(self, expression);
            break;
    }
}

void LMud_Compiler_CompileLoadVariable(struct LMud_Compiler* self, LMud_Any expression, enum LMud_BindingType type)
{
    LMud_Compiler_CompileVariable(self, expression, type, LMud_VariableMode_LOAD);
}

void LMud_Compiler_CompileStoreVariable(struct LMud_Compiler* self, LMud_Any expression, enum LMud_BindingType type)
{
    LMud_Compiler_CompileVariable(self, expression, type, LMud_VariableMode_STORE);
}

void LMud_Compiler_CompileLambda(struct LMud_Compiler* self, LMud_Any arglist, LMud_Any body)
{
    struct LMud_Compiler  subcompiler;
    LMud_Any              lambda;

    LMud_Compiler_Create_Lexical(&subcompiler, self);
    // TODO: Compile the variable prologue.
    (void) arglist;
    LMud_Compiler_CompileExpressions(&subcompiler, body);
    lambda = LMud_Compiler_Build(&subcompiler);
    LMud_Compiler_Destroy(&subcompiler);

    LMud_Compiler_WriteLambda(self, lambda);
}

void LMud_Compiler_CompileFunction(struct LMud_Compiler* self, LMud_Any expression)
{
    if (LMud_Lisp_IsSymbol(LMud_Compiler_GetLisp(self), expression))
        LMud_Compiler_CompileLoadVariable(self, expression, LMud_BindingType_FUNCTION);
    else
        LMud_Compiler_Compile(self, expression);
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

void LMud_Compiler_CompileSpecialQuote(struct LMud_Compiler* self, LMud_Any arguments)
{
    LMud_Any  value;

    /*
     * TODO: Error if arguments is not a list of length 1.
     */
    LMud_Lisp_TakeNext(LMud_Compiler_GetLisp(self), &arguments, &value);

    LMud_Compiler_CompileConstant(self, value);
}

void LMud_Compiler_CompileSpecialFunction(struct LMud_Compiler* self, LMud_Any arguments)
{
    LMud_Any  name;

    /*
     * TODO: Error if arguments is not a list of length 1.
     */
    LMud_Lisp_TakeNext(LMud_Compiler_GetLisp(self), &arguments, &name);

    LMud_Compiler_CompileFunction(self, name);
}

void LMud_Compiler_CompileSpecialLambda(struct LMud_Compiler* self, LMud_Any arguments)
{
    LMud_Any  arglist;
    LMud_Any  body;

    /*
     * TODO: Error if there is no arglist.
     */
    arglist = LMud_Lisp_Car(LMud_Compiler_GetLisp(self), arguments);
    body    = LMud_Lisp_Cdr(LMud_Compiler_GetLisp(self), arguments);

    LMud_Compiler_CompileLambda(self, arglist, body);
}

void LMud_Compiler_CompileSpecialProgn(struct LMud_Compiler* self, LMud_Any arguments)
{
    LMud_Compiler_CompileExpressions(self, arguments);
}

void LMud_Compiler_CompileSpecialSetq(struct LMud_Compiler* self, LMud_Any arguments)
{
    LMud_Any  variable;
    LMud_Any  value;

    /*
     * TODO: Error if arguments is not a list of length 2
     */
    LMud_Lisp_TakeNext(LMud_Compiler_GetLisp(self), &arguments, &variable);
    LMud_Lisp_TakeNext(LMud_Compiler_GetLisp(self), &arguments, &value);

    LMud_Compiler_Compile(self, value);
    LMud_Compiler_CompileStoreVariable(self, variable, LMud_BindingType_VARIABLE);
}


struct LMud_LetVariableInfo
{
    LMud_Any               name;
    struct LMud_Register*  reg;
};

void LMud_Compiler_CompileSpecialLet(struct LMud_Compiler* self, LMud_Any arguments)
{
    LMud_Any                      bindings;
    LMud_Any                      body;
    LMud_Any                      iterator;
    LMud_Size                     binding_count;
    LMud_Size                     index;
    struct LMud_LetVariableInfo*  variable_infos;

    bindings = LMud_Lisp_Car(LMud_Compiler_GetLisp(self), arguments);
    body     = LMud_Lisp_Cdr(LMud_Compiler_GetLisp(self), arguments);

    {
        binding_count = 0;

        for (iterator = bindings; LMud_Lisp_IsCons(LMud_Compiler_GetLisp(self), iterator); iterator = LMud_Lisp_Cdr(LMud_Compiler_GetLisp(self), iterator))
        {
            binding_count = binding_count + 1;
        }
    }

    variable_infos = LMud_Alloc(binding_count * sizeof(struct LMud_LetVariableInfo));

    {
        index = 0;

        for (iterator = bindings; LMud_Lisp_IsCons(LMud_Compiler_GetLisp(self), iterator); iterator = LMud_Lisp_Cdr(LMud_Compiler_GetLisp(self), iterator))
        {
            variable_infos[index].name  = LMud_Lisp_Car(LMud_Compiler_GetLisp(self), LMud_Lisp_Car(LMud_Compiler_GetLisp(self), iterator));
            variable_infos[index].reg   = LMud_Compiler_AllocateRegister(self);

            LMud_Compiler_CompileExpressions(self, LMud_Lisp_Cdr(LMud_Compiler_GetLisp(self), LMud_Lisp_Car(LMud_Compiler_GetLisp(self), iterator)));
            LMud_Compiler_WriteStoreRegister(self, variable_infos[index].reg);

            index = index + 1;
        }
    }

    LMud_Compiler_PushScope(self);
    {
        for (index = 0; index < binding_count; index++)
        {
            LMud_Compiler_BindRegister(self, variable_infos[index].name, LMud_BindingType_VARIABLE, variable_infos[index].reg);
        }

        LMud_Compiler_CompileExpressions(self, body);
    }
    LMud_Compiler_PopScope(self);

    LMud_Free(variable_infos);
}

void LMud_Compiler_CompileSpecialFlet(struct LMud_Compiler* self, LMud_Any arguments)
{
    LMud_Any                      bindings;
    LMud_Any                      body;
    LMud_Any                      iterator;
    LMud_Size                     binding_count;
    LMud_Size                     index;
    struct LMud_LetVariableInfo*  variable_infos;

    bindings = LMud_Lisp_Car(LMud_Compiler_GetLisp(self), arguments);
    body     = LMud_Lisp_Cdr(LMud_Compiler_GetLisp(self), arguments);

    {
        binding_count = 0;

        for (iterator = bindings; LMud_Lisp_IsCons(LMud_Compiler_GetLisp(self), iterator); iterator = LMud_Lisp_Cdr(LMud_Compiler_GetLisp(self), iterator))
        {
            binding_count = binding_count + 1;
        }
    }

    variable_infos = LMud_Alloc(binding_count * sizeof(struct LMud_LetVariableInfo));

    {
        index = 0;

        for (iterator = bindings; LMud_Lisp_IsCons(LMud_Compiler_GetLisp(self), iterator); iterator = LMud_Lisp_Cdr(LMud_Compiler_GetLisp(self), iterator))
        {
            variable_infos[index].name  = LMud_Lisp_Car(LMud_Compiler_GetLisp(self), LMud_Lisp_Car(LMud_Compiler_GetLisp(self), iterator));
            variable_infos[index].reg   = LMud_Compiler_AllocateRegister(self);

            LMud_Compiler_CompileSpecialLambda(self, LMud_Lisp_Cdr(LMud_Compiler_GetLisp(self), LMud_Lisp_Car(LMud_Compiler_GetLisp(self), iterator)));
            LMud_Compiler_WriteStoreRegister(self, variable_infos[index].reg);

            index = index + 1;
        }
    }

    LMud_Compiler_PushScope(self);
    {
        for (index = 0; index < binding_count; index++)
        {
            LMud_Compiler_BindRegister(self, variable_infos[index].name, LMud_BindingType_FUNCTION, variable_infos[index].reg);
        }

        LMud_Compiler_CompileExpressions(self, body);
    }
    LMud_Compiler_PopScope(self);

    LMud_Free(variable_infos);
}

void LMud_Compiler_CompileSpecialLabels(struct LMud_Compiler* self, LMud_Any arguments)
{
    /*
     * For now, treat this as a call to flet.
     */
    LMud_Compiler_CompileSpecialFlet(self, arguments);
}

void LMud_Compiler_CompileSpecialIf(struct LMud_Compiler* self, LMud_Any arguments)
{
    LMud_Any            condition;
    LMud_Any            consequent;
    LMud_Any            alternative;
    LMud_CompilerLabel  else_label;
    LMud_CompilerLabel  end_label;

    /*
     * TODO: Error if arguments is not a list of length 2 or 3
     */
    LMud_Lisp_TakeNext(LMud_Compiler_GetLisp(self), &arguments, &condition);
    LMud_Lisp_TakeNext(LMud_Compiler_GetLisp(self), &arguments, &consequent);
    LMud_Lisp_TakeNext(LMud_Compiler_GetLisp(self), &arguments, &alternative);

    LMud_Compiler_OpenLabel(self, &else_label);
    LMud_Compiler_OpenLabel(self, &end_label);

    LMud_Compiler_Compile(self, condition);

    LMud_Compiler_WriteJumpIfNil(self, else_label);

    LMud_Compiler_Compile(self, consequent);

    LMud_Compiler_WriteJump(self, end_label);

    LMud_Compiler_PlaceLabel(self, else_label);

    LMud_Compiler_Compile(self, alternative);

    LMud_Compiler_PlaceLabel(self, end_label);

    LMud_Compiler_CloseLabel(self, else_label);
    LMud_Compiler_CloseLabel(self, end_label);
}

void LMud_Compiler_CompileCombination(struct LMud_Compiler* self, LMud_Any expression)
{
    LMud_Any  function;
    LMud_Any  arguments;

    function  = LMud_Lisp_Car(LMud_Compiler_GetLisp(self), expression);
    arguments = LMud_Lisp_Cdr(LMud_Compiler_GetLisp(self), expression);

    /*
     * Handle special forms
     */
         if (LMud_Any_Eq(function, self->cached.symbol_quote))    LMud_Compiler_CompileSpecialQuote(self, arguments);
    else if (LMud_Any_Eq(function, self->cached.symbol_function)) LMud_Compiler_CompileSpecialFunction(self, arguments);
    else if (LMud_Any_Eq(function, self->cached.symbol_lambda))   LMud_Compiler_CompileSpecialLambda(self, arguments);
    else if (LMud_Any_Eq(function, self->cached.symbol_progn))    LMud_Compiler_CompileSpecialProgn(self, arguments);
    else if (LMud_Any_Eq(function, self->cached.symbol_setq))     LMud_Compiler_CompileSpecialSetq(self, arguments);
    else if (LMud_Any_Eq(function, self->cached.symbol_let))      LMud_Compiler_CompileSpecialLet(self, arguments);
    else if (LMud_Any_Eq(function, self->cached.symbol_flet))     LMud_Compiler_CompileSpecialFlet(self, arguments);
    else if (LMud_Any_Eq(function, self->cached.symbol_labels))   LMud_Compiler_CompileSpecialLabels(self, arguments);
    else if (LMud_Any_Eq(function, self->cached.symbol_if))       LMud_Compiler_CompileSpecialIf(self, arguments);
    else LMud_Compiler_CompileFuncall(self, function, arguments);
}

void LMud_Compiler_Compile(struct LMud_Compiler* self, LMud_Any expression)
{
    if (LMud_Lisp_IsSymbol(LMud_Compiler_GetLisp(self), expression))
        LMud_Compiler_CompileLoadVariable(self, expression, LMud_BindingType_VARIABLE);
    else if (LMud_Lisp_IsCons(LMud_Compiler_GetLisp(self), expression))
        LMud_Compiler_CompileCombination(self, expression);
    else
        LMud_Compiler_CompileConstant(self, expression);
}

void LMud_Compiler_CompileExpressions(struct LMud_Compiler* self, LMud_Any expressions)
{
    LMud_Any  expression;

    while (LMud_Lisp_IsCons(LMud_Compiler_GetLisp(self), expressions))
    {
        expression = LMud_Lisp_Car(LMud_Compiler_GetLisp(self), expressions);

        LMud_Compiler_Compile(self, expression);

        expressions = LMud_Lisp_Cdr(LMud_Compiler_GetLisp(self), expressions);
    }
}


LMud_Any LMud_Compiler_Build(struct LMud_Compiler* self)
{
    LMud_Compiler_WriteReturn(self);

    return LMud_Lisp_Function(
        LMud_Compiler_GetLisp(self),
        (struct LMud_ArgInfo) {
            .fixed_argument_count = 0, // TODO
            .stack_size           = self->max_stack_depth,
            .register_count       = self->max_register_index, // We keep an increment of 1
            .lexicalized          = self->uses_lexical_stuff,
            .variadic             = false, // TODO
        },
        LMud_Lisp_MakeBytes_FromData(LMud_Compiler_GetLisp(self), self->bytecodes_fill, (const char*) self->bytecodes),
        LMud_Lisp_MakeArray_FromData(LMud_Compiler_GetLisp(self), self->constants_fill, self->constants)
    );
}
