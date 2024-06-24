
#include <lmud/lisp/objects/builtin.h>
#include <lmud/lisp/runtime/fiber.h>

#include "builtins.h"


void LMud_Builtin_HelloWorld(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    (void) fiber;
    (void) arguments;
    (void) argument_count;

    printf("  Hello, world!\n");
}

void LMud_Builtin_SymbolValue(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Symbol_Value(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_SetSymbolValue(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Symbol_SetValue(LMud_Any_AsPointer(arguments[0]), arguments[1]);
    LMud_Fiber_SetAccumulator(fiber, arguments[1]);
}

void LMud_Builtin_SymbolFunction(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Symbol_Function(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_SetSymbolFunction(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Symbol_SetFunction(LMud_Any_AsPointer(arguments[0]), arguments[1]);
    LMud_Fiber_SetAccumulator(fiber, arguments[1]);
}

void LMud_Builtin_Cons(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Cons(fiber->lisp, arguments[0], arguments[1]));
}

void LMud_Builtin_Car(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Car(fiber->lisp, arguments[0]));
}

void LMud_Builtin_Cdr(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Cdr(fiber->lisp, arguments[0]));
}

void LMud_Builtin_List(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any  list;
    LMud_Size index;

    list = LMud_Lisp_Nil(fiber->lisp);

    index = argument_count;

    while (index --> 0)
    {
        list = LMud_Lisp_Cons(fiber->lisp, arguments[index], list);
    }

    LMud_Fiber_SetAccumulator(fiber, list);
}

void LMud_Builtin_Length(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any   sequence;
    LMud_Size  length;

    /*
     * TODO: Check arguments.
     */
    (void) argument_count;

    sequence = arguments[0];

    if (LMud_Lisp_IsArray(fiber->lisp, sequence))
        LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromInteger(LMud_Array_GetSize(LMud_Any_AsPointer(sequence))));
    else if (LMud_Lisp_IsBytes(fiber->lisp, sequence))
        LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromInteger(LMud_Bytes_GetSize(LMud_Any_AsPointer(sequence))));
    else if (LMud_Lisp_IsCons(fiber->lisp, sequence))
    {
        length = 0;

        while (LMud_Lisp_IsCons(fiber->lisp, sequence))
        {
            sequence = LMud_Lisp_Cdr(fiber->lisp, sequence);
            length++;
        }

        LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromInteger(length));
    }
    else
        LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromInteger(0));
}

void LMud_Builtin_Vector(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_MakeArray_FromData(fiber->lisp, argument_count, arguments));
}

void LMud_Builtin_Aref(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any  result;

    /*
     * TODO: Check arguments and the return value of `LMud_Lisp_Aref`.
     */
    (void) argument_count;

    LMud_Lisp_Aref(fiber->lisp, arguments[0], arguments[1], &result);

    LMud_Fiber_SetAccumulator(fiber, result);
}


void LMud_Lisp_InstallBuiltins(struct LMud_Lisp* lisp)
{
    LMud_Lisp_InstallBuiltin(lisp, "HELLO-WORLD", LMud_Builtin_HelloWorld);
    LMud_Lisp_InstallBuiltin(lisp, "SYMBOL-VALUE", LMud_Builtin_SymbolValue);
    LMud_Lisp_InstallBuiltin(lisp, "SET-SYMBOL-VALUE", LMud_Builtin_SetSymbolValue);
    LMud_Lisp_InstallBuiltin(lisp, "SYMBOL-FUNCTION", LMud_Builtin_SymbolFunction);
    LMud_Lisp_InstallBuiltin(lisp, "SET-SYMBOL-FUNCTION", LMud_Builtin_SetSymbolFunction);
    LMud_Lisp_InstallBuiltin(lisp, "CONS", LMud_Builtin_Cons);
    LMud_Lisp_InstallBuiltin(lisp, "CAR", LMud_Builtin_Car);
    LMud_Lisp_InstallBuiltin(lisp, "CDR", LMud_Builtin_Cdr);
    LMud_Lisp_InstallBuiltin(lisp, "LIST", LMud_Builtin_List);
    LMud_Lisp_InstallBuiltin(lisp, "LENGTH", LMud_Builtin_Length);
    LMud_Lisp_InstallBuiltin(lisp, "VECTOR", LMud_Builtin_Vector);
    LMud_Lisp_InstallBuiltin(lisp, "AREF", LMud_Builtin_Aref);
}
