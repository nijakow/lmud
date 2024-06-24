
#include <lmud/lisp/bytecodes.h>

#include <lmud/lisp/objects/array.h>
#include <lmud/lisp/objects/bytes.h>
#include <lmud/lisp/objects/function.h>
#include <lmud/lisp/objects/symbol.h>

#include <lmud/lisp/runtime/frame.h>
#include <lmud/lisp/runtime/fiber.h>

#include <lmud/lisp/lisp.h>

#include "interpreter.h"


struct LMud_InstructionStream
{
    struct LMud_Frame*     frame;
    char*                  ip;
    LMud_Any*              constants;
};

struct LMud_Function* LMud_InstructionStream_GetFunction(struct LMud_InstructionStream* self)
{
    return self->frame->function;
}

struct LMud_Bytes* LMud_InstructionStream_GetBytecodes(struct LMud_InstructionStream* self)
{
    return (struct LMud_Bytes*) LMud_Any_AsPointer(LMud_InstructionStream_GetFunction(self)->bytecodes);
}

struct LMud_Array* LMud_InstructionStream_GetConstants(struct LMud_InstructionStream* self)
{
    return (struct LMud_Array*) LMud_Any_AsPointer(LMud_InstructionStream_GetFunction(self)->constants);
}

void LMud_InstructionStream_Create(struct LMud_InstructionStream* self, struct LMud_Frame* frame)
{
    self->frame     = frame;
    self->ip        = LMud_Bytes_GetData(LMud_InstructionStream_GetBytecodes(self)) + frame->ip;
    self->constants = LMud_Array_GetData(LMud_InstructionStream_GetConstants(self));
}

void LMud_InstructionStream_Destroy(struct LMud_InstructionStream* self)
{
    self->frame->ip = self->ip - LMud_Bytes_GetData(LMud_InstructionStream_GetBytecodes(self));
}

void LMud_InstructionStream_Switch(struct LMud_InstructionStream* self, struct LMud_Frame* frame)
{
    LMud_InstructionStream_Destroy(self);
    LMud_InstructionStream_Create(self, frame);
}

void LMud_InstructionStream_Jump(struct LMud_InstructionStream* self, LMud_Size offset)
{
    self->ip = LMud_Bytes_GetData(LMud_InstructionStream_GetBytecodes(self)) + offset;
}

uint8_t LMud_InstructionStream_NextU8(struct LMud_InstructionStream* self)
{
    return (uint8_t) *self->ip++;
}

uint16_t LMud_InstructionStream_NextU16(struct LMud_InstructionStream* self)
{
    uint16_t value = *(uint16_t*) self->ip;
    self->ip += 2;
    return value;
}

enum LMud_Bytecode LMud_InstructionStream_NextBytecode(struct LMud_InstructionStream* self)
{
    return (enum LMud_Bytecode) LMud_InstructionStream_NextU8(self);
}

LMud_Any LMud_InstructionStream_NextConstant(struct LMud_InstructionStream* self)
{
    return self->constants[LMud_InstructionStream_NextU16(self)];
}

LMud_Size LMud_InstructionStream_NextJumpOffset(struct LMud_InstructionStream* self)
{
    return (LMud_Size) LMud_InstructionStream_NextU16(self);
}



void LMud_Interpreter_Create(struct LMud_Interpreter* self, struct LMud_Fiber* fiber)
{
    self->fiber = fiber;
}

void LMud_Interpreter_Destroy(struct LMud_Interpreter* self)
{
    (void) self;
}

struct LMud_Lisp* LMud_Interpreter_GetLisp(struct LMud_Interpreter* self)
{
    return self->fiber->lisp;
}

LMud_Any LMud_Interpreter_GetAccu(struct LMud_Interpreter* self)
{
    return LMud_Fiber_GetAccumulator(self->fiber);
}

void LMud_Interpreter_SetAccu(struct LMud_Interpreter* self, LMud_Any value)
{
    LMud_Fiber_SetAccumulator(self->fiber, value);
}

struct LMud_Frame* LMud_Interpreter_LexicalFrame(struct LMud_Interpreter* self, LMud_Size index)
{
    struct LMud_Frame* frame = self->fiber->top;

    while (index --> 0)
    {
        frame = frame->lexical;
    }

    return frame;
}


void LMud_Interpreter_Tick(struct LMud_Interpreter* self)
{
    struct LMud_InstructionStream  stream;
    struct LMud_Frame*             lexical;
    LMud_Any                       value;
    LMud_Size                      index;

    LMud_InstructionStream_Create(&stream, self->fiber->top);

    while (true)
    {
        switch (LMud_InstructionStream_NextBytecode(&stream))
        {
            case LMud_Bytecode_CONSTANT:
            {
                LMud_Interpreter_SetAccu(
                    self,
                    LMud_InstructionStream_NextConstant(&stream)
                );
                break;
            }

            case LMud_Bytecode_LAMBDA:
            {
                /*
                 * TODO
                 */
                break;
            }

            case LMud_Bytecode_SYMBOL_VARIABLE:
            {
                value = LMud_InstructionStream_NextConstant(&stream);

                if (!LMud_Lisp_IsSymbol(LMud_Interpreter_GetLisp(self), value))
                {
                    /*
                     * TODO
                     */
                }

                value = LMud_Symbol_Value((struct LMud_Symbol*) LMud_Any_AsPointer(value));

                LMud_Interpreter_SetAccu(
                    self,
                    value
                );

                break;
            }

            case LMud_Bytecode_SYMBOL_FUNCTION:
            {
                value = LMud_InstructionStream_NextConstant(&stream);

                if (!LMud_Lisp_IsSymbol(LMud_Interpreter_GetLisp(self), value))
                {
                    /*
                     * TODO
                     */
                }

                value = LMud_Symbol_Function((struct LMud_Symbol*) LMud_Any_AsPointer(value));

                LMud_Interpreter_SetAccu(
                    self,
                    value
                );

                break;
            }

            case LMud_Bytecode_LEXICAL_LOAD:
            {
                lexical = LMud_Interpreter_LexicalFrame(self, LMud_InstructionStream_NextU8(&stream));

                LMud_Interpreter_SetAccu(
                    self,
                    LMud_Frame_GetRegister(lexical, LMud_InstructionStream_NextU8(&stream))
                );

                break;
            }

            case LMud_Bytecode_LEXICAL_STORE:
            {
                lexical = LMud_Interpreter_LexicalFrame(self, LMud_InstructionStream_NextU8(&stream));

                LMud_Frame_SetRegister(
                    lexical,
                    LMud_InstructionStream_NextU8(&stream),
                    LMud_Interpreter_GetAccu(self)
                );

                break;
            }

            case LMud_Bytecode_PUSH:
            {
                LMud_Frame_Push(self->fiber->top, LMud_Interpreter_GetAccu(self));

                break;
            }

            case LMud_Bytecode_CALL:
            {
                /*
                 * TODO
                 */
                break;
            }

            case LMud_Bytecode_JUMP:
            {
                LMud_InstructionStream_Jump(&stream, LMud_InstructionStream_NextJumpOffset(&stream));

                break;
            }

            case LMud_Bytecode_JUMP_IF_NIL:
            {
                value = LMud_Interpreter_GetAccu(self);
                index = LMud_InstructionStream_NextJumpOffset(&stream);

                if (LMud_Lisp_IsNil(LMud_Interpreter_GetLisp(self), value))
                {
                    LMud_InstructionStream_Jump(&stream, index);
                }

                break;
            }

            case LMud_Bytecode_RETURN:
            {
                /*
                 * TODO
                 */

                break;
            }

            default:
            {
                /*
                 * TODO: Handle unknown bytecodes.
                 */
                break;
            }
        }
    }

    LMud_InstructionStream_Destroy(&stream);
}
