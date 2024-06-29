
#pragma once

#include <lmud/defs.h>

enum LMud_Bytecode
{
    LMud_Bytecode_NOP,
    LMud_Bytecode_HAS_ARGUMENT,
    LMud_Bytecode_POP_ARGUMENT,
    LMud_Bytecode_POP_KEYWORD_ARGUMENT,
    LMud_Bytecode_CONS_REST_ARGUMENTS,
    LMud_Bytecode_MULTIPLE_VALUE_LIST,
    
    LMud_Bytecode_LOAD_CONSTANT_SMALL,
    LMud_Bytecode_LOAD_CONSTANT,
    LMud_Bytecode_LOAD_LAMBDA,
    
    LMud_Bytecode_LOAD_REGISTER_LOCAL,
    LMud_Bytecode_LOAD_REGISTER_LEXICAL,
    LMud_Bytecode_LOAD_SYMBOL_VARIABLE,
    LMud_Bytecode_LOAD_SYMBOL_FUNCTION,

    LMud_Bytecode_STORE_REGISTER_LOCAL,
    LMud_Bytecode_STORE_REGISTER_LEXICAL,
    LMud_Bytecode_STORE_SYMBOL_VARIABLE,
    LMud_Bytecode_STORE_SYMBOL_FUNCTION,
    
    LMud_Bytecode_PUSH,
    LMud_Bytecode_CALL,

    LMud_Bytecode_JUMP,
    LMud_Bytecode_JUMP_IF_NIL,
    
    LMud_Bytecode_RETURN,
    
    LMud_Bytecode_SET_UNWIND_PROTECT,
    LMud_Bytecode_BEGIN_UNWIND_PROTECT,
    LMud_Bytecode_END_UNWIND_PROTECT,
    LMud_Bytecode_BEGIN_SIGNAL_HANDLER,
    LMud_Bytecode_SIGNAL,
};

enum LMud_ExecutionResumption
{
    LMud_ExecutionResumption_NORMAL,
    LMud_ExecutionResumption_DIRECT,
    LMud_ExecutionResumption_RETURN,
    LMud_ExecutionResumption_SIGNAL,
};
