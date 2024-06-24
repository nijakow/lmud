
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
}
