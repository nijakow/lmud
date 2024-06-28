
#include <lmud/lisp/bytecodes.h>

#include <lmud/lisp/objects/array.h>
#include <lmud/lisp/objects/bytes.h>
#include <lmud/lisp/objects/closure.h>
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

void LMud_InstructionStream_Flush(struct LMud_InstructionStream* self)
{
    self->frame->ip = self->ip - LMud_Bytes_GetData(LMud_InstructionStream_GetBytecodes(self));
    self->frame     = NULL;
}

void LMud_InstructionStream_Restore(struct LMud_InstructionStream* self, struct LMud_Frame* frame)
{
    assert(self->frame == NULL);
    self->frame     = frame;
    self->ip        = LMud_Bytes_GetData(LMud_InstructionStream_GetBytecodes(self)) + frame->ip;
    self->constants = LMud_Array_GetData(LMud_InstructionStream_GetConstants(self));
}

void LMud_InstructionStream_Create(struct LMud_InstructionStream* self, struct LMud_Frame* frame)
{
    self->frame = NULL;
    LMud_InstructionStream_Restore(self, frame);
}

void LMud_InstructionStream_Destroy(struct LMud_InstructionStream* self)
{
    if (self->frame != NULL)
        LMud_InstructionStream_Flush(self);
}

void LMud_InstructionStream_Switch(struct LMud_InstructionStream* self, struct LMud_Frame* frame)
{
    LMud_InstructionStream_Flush(self);
    LMud_InstructionStream_Restore(self, frame);
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
    struct LMud_Frame*  frame;
    
    frame = self->fiber->top;

    while (index --> 0)
    {
        frame = LMud_Frame_GetLexical(frame);
    }

    return frame;
}


#define LMud_Interpreter_Flush(self) \
    LMud_InstructionStream_Flush(&stream)
#define LMud_Interpreter_Restore(self) \
    LMud_InstructionStream_Restore(&stream, self->fiber->top)

#define TERMINATE \
    { \
        LMud_Fiber_Terminate(self->fiber); \
        goto end; \
    }

void LMud_Interpreter_Tick(struct LMud_Interpreter* self)
{
    struct LMud_InstructionStream  stream;
    struct LMud_Frame*             lexical;
    LMud_Any                       value;
    LMud_Any                       value2;
    LMud_Size                      index;
    LMud_Size                      index2;
    LMud_Size                      steps_remaining;

    steps_remaining = 64 * 1024;

    LMud_InstructionStream_Create(&stream, self->fiber->top);

    while (steps_remaining --> 0)
    {
        switch (LMud_InstructionStream_NextBytecode(&stream))
        {
            case LMud_Bytecode_NOP:
            {
                break;
            }

            case LMud_Bytecode_HAS_ARGUMENT:
            {
                LMud_Interpreter_SetAccu(
                    self,
                    LMud_Lisp_Boolean(
                        LMud_Interpreter_GetLisp(self),
                        LMud_Frame_HasExtraArguments(self->fiber->top)
                    )
                );
                break;
            }

            case LMud_Bytecode_POP_ARGUMENT:
            {
                if (!LMud_Frame_TakeExtraArgument(self->fiber->top, &value))
                    value = LMud_Lisp_Nil(LMud_Interpreter_GetLisp(self));
                LMud_Interpreter_SetAccu(self, value);
                break;
            }

            case LMud_Bytecode_POP_KEYWORD_ARGUMENT:
            {
                value = LMud_InstructionStream_NextConstant(&stream);
                index = LMud_InstructionStream_NextJumpOffset(&stream);

                if (LMud_Frame_PickKeywordArgument(self->fiber->top, value, &value2))
                {
                    /*
                     * Set the accumulator to the value...
                     */
                    LMud_Interpreter_SetAccu(self, value2);

                    /*
                     * ...and jump to the target instruction.
                     */
                    LMud_InstructionStream_Jump(&stream, index);
                }

                break;
            }

            case LMud_Bytecode_CONS_REST_ARGUMENTS:
            {
                value = LMud_Lisp_Nil(LMud_Interpreter_GetLisp(self));
                index = LMud_Frame_RemainingExtraArgumentCount(self->fiber->top);

                while (index --> 0)
                {
                    LMud_Frame_GetExtraArgument(self->fiber->top, index, &value2);
                    value = LMud_Lisp_Cons(LMud_Interpreter_GetLisp(self), value2, value);
                }

                LMud_Interpreter_SetAccu(self, value);

                break;
            }

            case LMud_Bytecode_MULTIPLE_VALUE_LIST:
            {
                value = LMud_Lisp_Nil(LMud_Interpreter_GetLisp(self));
                index = LMud_Fiber_ValueCount(self->fiber);

                while (index --> 0)
                {
                    value2 = LMud_Fiber_GetValue(self->fiber, index);
                    value  = LMud_Lisp_Cons(LMud_Interpreter_GetLisp(self), value2, value);
                }

                LMud_Interpreter_SetAccu(self, value);

                break;
            }

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
                value = LMud_InstructionStream_NextConstant(&stream);

                if (!LMud_Lisp_IsFunction(LMud_Interpreter_GetLisp(self), value))
                {
                    /*
                     * TODO
                     */
                }

                if (LMud_Function_IsLexicalized((struct LMud_Function*) LMud_Any_AsPointer(value))) {
                    LMud_Interpreter_SetAccu(
                        self,
                        LMud_Lisp_Closure(
                            LMud_Interpreter_GetLisp(self),
                            (struct LMud_Function*) LMud_Any_AsPointer(value),
                            self->fiber->top
                        )
                    );
                } else {
                    LMud_Interpreter_SetAccu(
                        self,
                        value
                    );
                }

                break;
            }

            case LMud_Bytecode_SYMBOL_VARIABLE_LOAD:
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

            case LMud_Bytecode_SYMBOL_VARIABLE_STORE:
            {
                value = LMud_InstructionStream_NextConstant(&stream);

                if (!LMud_Lisp_IsSymbol(LMud_Interpreter_GetLisp(self), value))
                {
                    /*
                     * TODO
                     */
                }

                LMud_Symbol_SetValue(
                    (struct LMud_Symbol*) LMud_Any_AsPointer(value),
                    LMud_Interpreter_GetAccu(self)
                );

                break;
            }

            case LMud_Bytecode_SYMBOL_FUNCTION_LOAD:
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

            case LMud_Bytecode_SYMBOL_FUNCTION_STORE:
            {
                value = LMud_InstructionStream_NextConstant(&stream);

                if (!LMud_Lisp_IsSymbol(LMud_Interpreter_GetLisp(self), value))
                {
                    /*
                     * TODO
                     */
                }

                LMud_Symbol_SetFunction(
                    (struct LMud_Symbol*) LMud_Any_AsPointer(value),
                    LMud_Interpreter_GetAccu(self)
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
                index = LMud_InstructionStream_NextU8(&stream);
                value = LMud_Interpreter_GetAccu(self);

                LMud_Interpreter_Flush(self);
                LMud_Fiber_PerformCall(self->fiber, value, index);
                LMud_Interpreter_Restore(self);

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
                LMud_Interpreter_Flush(self);
                LMud_Fiber_PerformReturn(self->fiber);
                if (!LMud_Fiber_HasFrames(self->fiber))
                    TERMINATE;
                LMud_Interpreter_Restore(self);
                break;
            }

            case LMud_Bytecode_SET_UNWIND_PROTECT:
            {
                index = LMud_InstructionStream_NextJumpOffset(&stream);

                LMud_Frame_SetUnwindProtect(self->fiber->top, index);

                break;
            }

            case LMud_Bytecode_BEGIN_UNWIND_PROTECT:
            {
                /*
                 * First, we restore the stack pointer.
                 */
                LMud_Frame_SetStackPointer(self->fiber->top, LMud_InstructionStream_NextU8(&stream));

                /*
                 * Then, we push the accumulator/values state.
                 */
                if (LMud_Fiber_ValueCount(self->fiber) == 1) {
                    /*
                     * This is a special case.
                     */
                    LMud_Frame_Push(self->fiber->top, LMud_Fiber_GetAccumulator(self->fiber));
                } else {
                    /*
                     * This is the general case.
                     */
                    value = LMud_Lisp_Nil(self->fiber->lisp);
                    index = LMud_Fiber_ValueCount(self->fiber);

                    while (index --> 0)
                    {
                        value = LMud_Lisp_Cons(self->fiber->lisp, LMud_Fiber_GetValue(self->fiber, index), value);
                    }

                    LMud_Frame_Push(self->fiber->top, value);
                }

                LMud_Frame_Push(self->fiber->top, LMud_Any_FromInteger(LMud_Fiber_ValueCount(self->fiber)));

                break;
            }

            case LMud_Bytecode_END_UNWIND_PROTECT:
            {
                /*
                 * We restore the accumulator/values from the stack.
                 */
                index2 = LMud_Any_AsInteger(LMud_Frame_Pop(self->fiber->top));
                value  = LMud_Frame_Pop(self->fiber->top);

                if (index2 == 1) {
                    /*
                     * The special case.
                     */
                    LMud_Fiber_SetAccumulator(self->fiber, value);
                } else {
                    /*
                     * The general case.
                     */

                    LMud_Any  values[index2];

                    for (index = 0; index < index2; index++)
                    {
                        values[index] = LMud_Lisp_Car(self->fiber->lisp, value);
                        value         = LMud_Lisp_Cdr(self->fiber->lisp, value);
                    }

                    LMud_Fiber_Values(self->fiber, values, index2);
                }
                
                break;
            }

            default:
            {
                LMud_Interpreter_Flush(self);
                LMud_Fiber_PerformError(self->fiber, "Invalid bytecode!");
                TERMINATE;
            }
        }
    }

  end:
    LMud_InstructionStream_Destroy(&stream);
}
