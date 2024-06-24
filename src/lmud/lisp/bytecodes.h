
#pragma once

#include <lmud/defs.h>


enum LMud_Bytecode
{
    LMud_Bytecode_CONSTANT,
    LMud_Bytecode_LAMBDA,
    LMud_Bytecode_SYMBOL_VARIABLE,
    LMud_Bytecode_SYMBOL_FUNCTION,
    LMud_Bytecode_LEXICAL_LOAD,
    LMud_Bytecode_LEXICAL_STORE,
    LMud_Bytecode_PUSH,
    LMud_Bytecode_CALL,
    LMud_Bytecode_JUMP,
    LMud_Bytecode_JUMP_IF_NIL,
    LMud_Bytecode_RETURN,
};
