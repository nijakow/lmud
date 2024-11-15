
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
    assert(frame       != NULL);
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

LMud_Any LMud_InstructionStream_NextSmallConstant(struct LMud_InstructionStream* self)
{
    return self->constants[LMud_InstructionStream_NextU8(self)];
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

static void LMud_Interpreter_PerformError(struct LMud_Interpreter* self, const char* message)
{
    LMud_Fiber_PerformError(self->fiber, message);
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
        goto end; \
    }

#define AGAIN_WITH_CHECK \
    { \
        goto again_with_check; \
    }

#define LMud_Interpreter_ERROR(message) \
    { \
        LMud_Interpreter_Flush(self); \
        LMud_Interpreter_PerformError(self, message); \
        LMud_Interpreter_Restore(self); \
        AGAIN_WITH_CHECK; \
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
    enum LMud_ExecutionResumption  resumption;

    steps_remaining = 64 * 1024;

    LMud_InstructionStream_Create(&stream, self->fiber->top);

  again_with_check:
    if (!LMud_Fiber_IsRunning(self->fiber))
        goto end;

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

            case LMud_Bytecode_MULTIPLE_VALUE_BIND:
            {
                index2 = LMud_InstructionStream_NextU8(&stream);

                for (index = 0; index < index2; index++)
                {
                    LMud_Frame_SetRegister(
                        self->fiber->top,
                        LMud_InstructionStream_NextU8(&stream),
                        LMud_Fiber_GetValue(self->fiber, index)
                    );
                }

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

            case LMud_Bytecode_LOAD_CONSTANT_SMALL:
            {
                LMud_Interpreter_SetAccu(
                    self,
                    LMud_InstructionStream_NextSmallConstant(&stream)
                );
                break;
            }

            case LMud_Bytecode_LOAD_CONSTANT:
            {
                LMud_Interpreter_SetAccu(
                    self,
                    LMud_InstructionStream_NextConstant(&stream)
                );
                break;
            }

            case LMud_Bytecode_LOAD_LAMBDA:
            {
                value = LMud_InstructionStream_NextConstant(&stream);

                if (!LMud_Lisp_IsFunction(LMud_Interpreter_GetLisp(self), value))
                {
                    LMud_Interpreter_ERROR("Invalid function!");
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

            case LMud_Bytecode_LOAD_SYMBOL_VARIABLE:
            {
                value = LMud_InstructionStream_NextConstant(&stream);

                if (!LMud_Lisp_IsSymbol(LMud_Interpreter_GetLisp(self), value))
                {
                    LMud_Interpreter_ERROR("Invalid symbol!");
                }

                value = LMud_Symbol_Value((struct LMud_Symbol*) LMud_Any_AsPointer(value));

                LMud_Interpreter_SetAccu(
                    self,
                    value
                );

                break;
            }

            case LMud_Bytecode_STORE_SYMBOL_VARIABLE:
            {
                value = LMud_InstructionStream_NextConstant(&stream);

                if (!LMud_Lisp_IsSymbol(LMud_Interpreter_GetLisp(self), value))
                {
                    LMud_Interpreter_ERROR("Invalid symbol!");
                }

                if (!LMud_Symbol_SetValue((struct LMud_Symbol*) LMud_Any_AsPointer(value), LMud_Interpreter_GetAccu(self), false))
                {
                    LMud_Interpreter_ERROR("Symbol is locked!");
                }

                break;
            }

            case LMud_Bytecode_LOAD_SYMBOL_FUNCTION:
            {
                value = LMud_InstructionStream_NextConstant(&stream);

                if (!LMud_Lisp_IsSymbol(LMud_Interpreter_GetLisp(self), value))
                {
                    LMud_Interpreter_ERROR("Invalid symbol!");
                }

                value = LMud_Symbol_Function((struct LMud_Symbol*) LMud_Any_AsPointer(value));

                LMud_Interpreter_SetAccu(
                    self,
                    value
                );

                break;
            }

            case LMud_Bytecode_STORE_SYMBOL_FUNCTION:
            {
                value = LMud_InstructionStream_NextConstant(&stream);

                if (!LMud_Lisp_IsSymbol(LMud_Interpreter_GetLisp(self), value))
                {
                    LMud_Interpreter_ERROR("Invalid symbol!");
                }

                if (!LMud_Symbol_SetFunction((struct LMud_Symbol*) LMud_Any_AsPointer(value), LMud_Interpreter_GetAccu(self), false))
                {
                    LMud_Interpreter_ERROR("Symbol is locked!");
                }

                break;
            }

            case LMud_Bytecode_LOAD_REGISTER_LOCAL:
            {
                LMud_Interpreter_SetAccu(
                    self,
                    LMud_Frame_GetRegister(self->fiber->top, LMud_InstructionStream_NextU8(&stream))
                );

                break;
            }

            case LMud_Bytecode_LOAD_REGISTER_LEXICAL:
            {
                lexical = LMud_Interpreter_LexicalFrame(self, LMud_InstructionStream_NextU8(&stream));

                LMud_Interpreter_SetAccu(
                    self,
                    LMud_Frame_GetRegister(lexical, LMud_InstructionStream_NextU8(&stream))
                );

                break;
            }

            case LMud_Bytecode_STORE_REGISTER_LOCAL:
            {
                LMud_Frame_SetRegister(
                    self->fiber->top,
                    LMud_InstructionStream_NextU8(&stream),
                    LMud_Interpreter_GetAccu(self)
                );

                break;
            }

            case LMud_Bytecode_STORE_REGISTER_LEXICAL:
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
                if (!LMud_Fiber_HasFrames(self->fiber))
                    TERMINATE;
                LMud_Interpreter_Restore(self);

                AGAIN_WITH_CHECK;
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
                LMud_Frame_SetStackPointerRelativeToStackOrigin(self->fiber->top, LMud_InstructionStream_NextU8(&stream));

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

                /*
                 * The value count is pushed next. This is the second of the three magic values.
                 */
                LMud_Frame_Push(self->fiber->top, LMud_Any_FromInteger(LMud_Fiber_ValueCount(self->fiber)));

                /*
                 * We push the resumption mode. This is the last of the three magic values
                 * needed for the unwinding process.
                 */
                LMud_Frame_Push(self->fiber->top, LMud_Any_FromInteger(LMud_Fiber_GetExecutionResumptionMode(self->fiber)));

                /*
                 * We set our execution resumption mode back to NORMAL, so that the unwinding
                 * code can run without interference.
                 */
                LMud_Fiber_SetExecutionResumptionMode(self->fiber, LMud_ExecutionResumption_NORMAL);

                break;
            }

            case LMud_Bytecode_END_UNWIND_PROTECT:
            {
                LMud_Interpreter_Flush(self);
                LMud_Interpreter_Restore(self);

                /*
                 * First, we pop the resumption mode.
                 */
                resumption = (enum LMud_ExecutionResumption) LMud_Any_AsInteger(LMud_Frame_Pop(self->fiber->top));

                /*
                 * We restore the accumulator/values from the stack.
                 */
                index2 = LMud_Any_AsInteger(LMud_Frame_Pop(self->fiber->top));
                value  = LMud_Frame_Pop(self->fiber->top);

                /*
                 * In order to load the values, we need a dynamic array.
                 */
                LMud_Any  values[index2];

                if (index2 == 0) {
                    // Do nothing.
                } else if (index2 == 1) {
                    /*
                     * The special case.
                     */
                    values[0] = value;
                } else {
                    /*
                     * The general case.
                     */

                    for (index = 0; index < index2; index++)
                    {
                        values[index] = LMud_Lisp_Car(self->fiber->lisp, value);
                        value         = LMud_Lisp_Cdr(self->fiber->lisp, value);
                    }
                }

                /*
                 * Unwind-protect blocks can run as part of normal execution flow, but some of them
                 * may also be triggered by signals. If our current unwind-protect block is run as
                 * part of a stack-unwinding signal propagation, we should not resume the execution
                 * of our function, but unwind further and skip directly to the next
                 * unwind-protect block surrounding us.
                 * 
                 * In order to get this right, we also need to push and pop some information about
                 * whether we are coming from normal code or from an unwinding process, and not
                 * just the state of the registers:
                 * 
                 * if (coming_from_signal_unwind) {
                 *     // Unwind further (the signal info can be found in the accumulator)
                 * } else {
                 *     // Do nothing, we can continue our normal execution
                 * }
                 */
                switch (resumption) {
                    case LMud_ExecutionResumption_NORMAL:
                        /*
                         * We set the values, and continue our normal execution.
                         */
                        LMud_Fiber_Values(self->fiber, values, index2);
                        break;
                    case LMud_ExecutionResumption_DIRECT:
                        /*
                         * We do nothing, and continue our normal execution.
                         */
                        LMud_Fiber_SetExecutionResumptionMode(self->fiber, LMud_ExecutionResumption_NORMAL);
                        break;
                    case LMud_ExecutionResumption_SIGNAL:
                        /*
                         * We set the values, and signal again.
                         */
                        LMud_Interpreter_Flush(self);
                        LMud_Fiber_Values(self->fiber, values, index2);
                        LMud_Fiber_SignalAndUnwind(self->fiber);
                        LMud_Interpreter_Restore(self);
                        break;
                    case LMud_ExecutionResumption_TERMINATING:
                        /*
                         * We set the values, and continue terminating the fiber.
                         */
                        LMud_Interpreter_Flush(self);
                        LMud_Fiber_ContinueTerminate_FRIEND(self->fiber);
                        LMud_Interpreter_Restore(self);
                        break;
                    default:
                        LMud_Interpreter_ERROR("Execution resumption mode not recognized!");
                        break;
                }
                
                break;
            }

            case LMud_Bytecode_BEGIN_SIGNAL_HANDLER:
            {
                /*
                 * This instruction can only occur in the context of an unwind-protect block,
                 * directly after the BEGIN-UNWIND-PROTECT instruction.
                 * 
                 * The BEGIN-SIGNAL-HANDLER instruction is used in unwind-protect blocks established
                 * to handle signals, and therefore interfers with the "invisible pass-through"
                 * of unwind-protect blocks.
                 * 
                 * To make a signal handler block actually return something, we need to change the
                 * execution resumption mode that was left on the stack by BEGIN-UNWIND-PROTECT.
                 */

                /*
                 * Fetch our arguments.
                 */
                index  = LMud_InstructionStream_NextU8(&stream);
                index2 = LMud_InstructionStream_NextJumpOffset(&stream);

                /*
                 * Fetch the execution resumption mode.
                 */
                resumption = (enum LMud_ExecutionResumption) LMud_Any_AsInteger(LMud_Frame_Pop(self->fiber->top));

                /*
                 * If we are handling a signal, we bind the signalled value
                 * to the register. The resumption mode is changed to DIRECT,
                 * so that the return value of the handler block will not be
                 * overwritten by the restored accumulator.
                 * 
                 * Otherwise, we jump to the target instruction that skips
                 * the signal handler.
                 */
                if (resumption == LMud_ExecutionResumption_SIGNAL) {
                    resumption = LMud_ExecutionResumption_DIRECT;  // The value will be pushed later in this block.
                    LMud_Frame_SetRegister(self->fiber->top, index, LMud_Interpreter_GetAccu(self)); 
                } else {
                    LMud_InstructionStream_Jump(&stream, index2);
                }

                /*
                 * Push the (possibly new) execution resumption mode back onto the stack.
                 */
                LMud_Frame_Push(self->fiber->top, LMud_Any_FromInteger(resumption));

                break;
            }

            default:
            {
                LMud_Interpreter_Flush(self);
                LMud_Interpreter_ERROR("Invalid bytecode!");
            }
        }
    }

  end:
    LMud_InstructionStream_Destroy(&stream);
    LMud_Debugf(self->fiber->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Interpreter ticked %lu steps", (64 * 1024 - steps_remaining));
}
