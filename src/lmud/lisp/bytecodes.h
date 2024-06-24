
#pragma once

#include <lmud/defs.h>


enum LMud_Bytecode
{
    LMud_Bytecode_CONSTANT,
    LMud_Bytecode_PUSH,
    LMud_Bytecode_CALL,
    LMud_Bytecode_JUMP,
    LMud_Bytecode_JUMP_IF_NIL,
    LMud_Bytecode_RETURN,
};
