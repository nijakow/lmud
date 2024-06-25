
#pragma once

#include <lmud/defs.h>


enum LMud_Bytecode
{
    LMud_Bytecode_NOP,
    LMud_Bytecode_HAS_ARGUMENT,
    LMud_Bytecode_POP_ARGUMENT,
    LMud_Bytecode_CONSTANT,
    LMud_Bytecode_LAMBDA,
    LMud_Bytecode_SYMBOL_VARIABLE_LOAD,
    LMud_Bytecode_SYMBOL_VARIABLE_STORE,
    LMud_Bytecode_SYMBOL_FUNCTION_LOAD,
    LMud_Bytecode_SYMBOL_FUNCTION_STORE,
    LMud_Bytecode_LEXICAL_LOAD,
    LMud_Bytecode_LEXICAL_STORE,
    LMud_Bytecode_PUSH,
    LMud_Bytecode_CALL,
    LMud_Bytecode_JUMP,
    LMud_Bytecode_JUMP_IF_NIL,
    LMud_Bytecode_RETURN,
};
