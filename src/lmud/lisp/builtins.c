
#include <lmud/lisp/objects/builtin.h>
#include <lmud/lisp/runtime/fiber.h>
#include <lmud/lisp/math.h>
#include <lmud/lisp/io.h>
#include <lmud/util/stringbuilder.h>
#include <lmud/util/utf8.h>

#include "builtins.h"


void LMud_Builtin_HelloWorld(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    (void) fiber;
    (void) arguments;
    (void) argument_count;

    printf("  Hello, world!\n");
}

void LMud_Builtin_Quit(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    (void) fiber;
    (void) arguments;
    (void) argument_count;

    printf("\n<<QUIT BUILTIN WAS TRIGGERED>>\n");
    exit(0);
}

void LMud_Builtin_Funcall(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Fiber_Enter(fiber, arguments[0], &arguments[1], argument_count - 1);
}

void LMud_Builtin_Apply(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any   function;
    LMud_Any   iterator;
    LMud_Size  extra_argument_count;
    LMud_Size  index;

    /*
     * TODO: Check arguments.
     */

    function             = arguments[0];
    extra_argument_count = 0;
    iterator             = arguments[argument_count - 1];

    while (LMud_Lisp_IsCons(fiber->lisp, iterator))
    {
        extra_argument_count++;
        iterator = LMud_Lisp_Cdr(fiber->lisp, iterator);
    }

    LMud_Any  args[argument_count + extra_argument_count];

    for (index = 0; index < argument_count - 2; index++)
    {
        args[index] = arguments[index + 1];
    }

    iterator = arguments[argument_count - 1];

    while (LMud_Lisp_IsCons(fiber->lisp, iterator))
    {
        args[index++] = LMud_Lisp_Car(fiber->lisp, iterator);
        iterator      = LMud_Lisp_Cdr(fiber->lisp, iterator);
    }

    LMud_Fiber_Enter(fiber, function, args, index);
}

void LMud_Builtin_Values(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Fiber_Values(fiber, arguments, argument_count);
}

void LMud_Builtin_Symbolp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsSymbol(fiber->lisp, arguments[0])));
}

void LMud_Builtin_Gensym(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) arguments;
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Gensym(fiber->lisp));
}

void LMud_Builtin_SymbolPackage(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Symbol_Package(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_SymbolName(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Symbol_Name(LMud_Any_AsPointer(arguments[0])));
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

void LMud_Builtin_SymbolMacro(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Symbol_Macro(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_SetSymbolMacro(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Symbol_SetMacro(LMud_Any_AsPointer(arguments[0]), arguments[1]);
    LMud_Fiber_SetAccumulator(fiber, arguments[1]);
}

void LMud_Builtin_SymbolPlist(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Symbol_Plist(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_SetSymbolPlist(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Symbol_SetPlist(LMud_Any_AsPointer(arguments[0]), arguments[1]);
    LMud_Fiber_SetAccumulator(fiber, arguments[1]);
}

void LMud_Builtin_Packagep(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsPackage(fiber->lisp, arguments[0])));
}

void LMud_Builtin_PackageName(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Package_Name(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_Consp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsCons(fiber->lisp, arguments[0])));
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

void LMud_Builtin_Rplaca(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Lisp_Rplaca(fiber->lisp, arguments[0], arguments[1]);
    LMud_Fiber_SetAccumulator(fiber, arguments[1]);
}

void LMud_Builtin_Rplacd(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Lisp_Rplacd(fiber->lisp, arguments[0], arguments[1]);
    LMud_Fiber_SetAccumulator(fiber, arguments[1]);
}

void LMud_Builtin_Customp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsCustom(fiber->lisp, arguments[0])));
}

void LMud_Builtin_MakeCustom(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check argument count, must be at least 1.
     */
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Custom(fiber->lisp, arguments[0], &arguments[1], argument_count - 1));
}

void LMud_Builtin_CustomMeta(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Custom_Meta(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_CustomSetMeta(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Custom_SetMeta(LMud_Any_AsPointer(arguments[0]), arguments[1]);
    LMud_Fiber_SetAccumulator(fiber, arguments[1]);
}

void LMud_Builtin_CustomSize(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromInteger(LMud_Custom_Size(LMud_Any_AsPointer(arguments[0]))));
}

void LMud_Builtin_CustomAt(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Custom_At(LMud_Any_AsPointer(arguments[0]), LMud_Any_AsInteger(arguments[1])));
}

void LMud_Builtin_CustomSet(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Custom_Set(LMud_Any_AsPointer(arguments[0]), LMud_Any_AsInteger(arguments[1]), arguments[2]);
    LMud_Fiber_SetAccumulator(fiber, arguments[2]);
}

void LMud_Builtin_Eq(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Size  index;

    for (index = 1; index < argument_count; index++)
    {
        if (!LMud_Any_Eq(arguments[0], arguments[index]))
        {
            LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Nil(fiber->lisp));
            return;
        }
    }

    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_T(fiber->lisp));
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

void LMud_Builtin_ListStar(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any  list;
    LMud_Size index;

    /*
     * TODO: Check arguments.
     */

    list = arguments[argument_count - 1];

    index = argument_count - 1;

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
    else if (LMud_Lisp_IsString(fiber->lisp, sequence))
        LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromInteger(LMud_String_RuneLength(LMud_Any_AsPointer(sequence))));
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

void LMud_Builtin_Bytesp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsBytes(fiber->lisp, arguments[0])));
}

void LMud_Builtin_Bytes(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Size           index;
    char                data[argument_count];

    for (index = 0; index < argument_count; index++)
    {
        data[index] = LMud_Any_AsInteger(arguments[index]);
    }

    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_MakeBytes_FromData(fiber->lisp, argument_count, data));
}


void LMud_Builtin_Vectorp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsArray(fiber->lisp, arguments[0])));
}

void LMud_Builtin_Vector(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_MakeArray_FromData(fiber->lisp, argument_count, arguments));
}

void LMud_Builtin_Stringp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsString(fiber->lisp, arguments[0])));
}

void LMud_Builtin_String(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_StringBuilder  builder;
    struct LMud_Utf8_Encoder   encoder;
    LMud_Any                   result;
    LMud_Size                  index;

    /*
     * TODO: Check arguments.
     */

    LMud_StringBuilder_Create(&builder);
    {
        for (index = 0; index < argument_count; index++)
        {
            if (!LMud_Any_IsCharacter(arguments[index]))
                continue;
            
            LMud_Utf8_Encoder_Create(&encoder, LMud_Any_AsCharacter(arguments[index]));
            LMud_StringBuilder_AppendCStr(&builder, LMud_Utf8_Encoder_AsString(&encoder));
            LMud_Utf8_Encoder_Destroy(&encoder);
        }

        result = LMud_Lisp_String(fiber->lisp, LMud_StringBuilder_GetStatic(&builder));
    }
    LMud_StringBuilder_Destroy(&builder);

    LMud_Fiber_SetAccumulator(fiber, result);
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

void LMud_Builtin_Aset(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments and the return value of `LMud_Lisp_Aset`.
     */
    (void) argument_count;

    LMud_Lisp_Aset(fiber->lisp, arguments[0], arguments[1], arguments[2]);
    LMud_Fiber_SetAccumulator(fiber, arguments[2]);
}

void LMud_Builtin_BytecodeFunctionp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsFunction(fiber->lisp, arguments[0])));
}

void LMud_Builtin_FunctionBytecodes(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Function_Bytecodes(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_FunctionConstants(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Function_Constants(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_Compile(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any  result;

    /*
     * TODO: Check arguments and whether the compilation was successful.
     */
    (void) argument_count;

    LMud_Lisp_Compile(fiber->lisp, arguments[0], &result);

    LMud_Fiber_SetAccumulator(fiber, result);
}


void LMud_Builtin_Read(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any  result;

    (void) arguments;
    (void) argument_count;

    if (!LMud_Lisp_Read(fiber->lisp, &fiber->lisp->standard_input, &result))
        result = LMud_Lisp_Nil(fiber->lisp);

    LMud_Fiber_SetAccumulator(fiber, result);
}

void LMud_Builtin_Princ(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;

    LMud_Lisp_Print(fiber->lisp, arguments[0], stdout, false);

    LMud_Fiber_SetAccumulator(fiber, arguments[0]);
}

void LMud_Builtin_Prin1(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;

    LMud_Lisp_Print(fiber->lisp, arguments[0], stdout, true);

    LMud_Fiber_SetAccumulator(fiber, arguments[0]);
}

void LMud_Builtin_Terpri(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    (void) fiber;
    (void) arguments;
    (void) argument_count;

    printf("\n");
}


void LMud_Builtin_Characterp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Any_IsCharacter(arguments[0])));
}

void LMud_Builtin_CharCode(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;

    LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromInteger(LMud_Rune_AsInteger(LMud_Any_AsCharacter(arguments[0]))));
}

void LMud_Builtin_CodeChar(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;

    LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromCharacter(LMud_Rune_FromInteger(LMud_Any_AsInteger(arguments[0]))));
}


void LMud_Builtin_Numerator(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any  result;

    /*
     * TODO: Check arguments.
     */
    (void) argument_count;

    result = LMud_Lisp_Numerator(fiber->lisp, arguments[0]);

    LMud_Fiber_SetAccumulator(fiber, result);
}

void LMud_Builtin_Denominator(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any  result;

    /*
     * TODO: Check arguments.
     */
    (void) argument_count;

    result = LMud_Lisp_Denominator(fiber->lisp, arguments[0]);

    LMud_Fiber_SetAccumulator(fiber, result);
}

static void LMud_Builtin_NumericComparison(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count, bool (*comparison)(struct LMud_Lisp*, LMud_Any, LMud_Any))
{
    LMud_Any   value;
    LMud_Size  index;

    /*
     * TODO: Check arguments.
     */
    value = arguments[0];

    for (index = 1; index < argument_count; index++)
    {
        if (!comparison(fiber->lisp, value, arguments[index]))
        {
            LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Nil(fiber->lisp));
            return;
        }

        value = arguments[index];
    }

    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_T(fiber->lisp));
}

void LMud_Builtin_NumericEqual(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Builtin_NumericComparison(fiber, arguments, argument_count, LMud_Lisp_NumericEqual);
}

void LMud_Builtin_NumericLess(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Builtin_NumericComparison(fiber, arguments, argument_count, LMud_Lisp_NumericLess);
}

void LMud_Builtin_NumericLessEqual(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Builtin_NumericComparison(fiber, arguments, argument_count, LMud_Lisp_NumericLessEqual);
}

void LMud_Builtin_NumericGreater(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Builtin_NumericComparison(fiber, arguments, argument_count, LMud_Lisp_NumericGreater);
}

void LMud_Builtin_NumericGreaterEqual(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Builtin_NumericComparison(fiber, arguments, argument_count, LMud_Lisp_NumericGreaterEqual);
}

void LMud_Builtin_Plus(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any   result;
    LMud_Size  index;

    /*
     * TODO: Check arguments.
     */
    
    result = LMud_Any_FromInteger(0);

    for (index = 0; index < argument_count; index++)
    {
        LMud_Lisp_Add2(fiber->lisp, result, arguments[index], &result);
    }

    LMud_Fiber_SetAccumulator(fiber, result);
}

void LMud_Builtin_Minus(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any   result;
    LMud_Size  index;

    /*
     * TODO: Check arguments.
     */

    if (argument_count == 0) {
        result = LMud_Any_FromInteger(0);
    } else if (argument_count == 1) {
        LMud_Lisp_Sub2(fiber->lisp, LMud_Any_FromInteger(0), arguments[0], &result);
    } else {
        result = arguments[0];

        for (index = 1; index < argument_count; index++)
        {
            LMud_Lisp_Sub2(fiber->lisp, result, arguments[index], &result);
        }
    }

    LMud_Fiber_SetAccumulator(fiber, result);
}

void LMud_Builtin_Multiply(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any   result;
    LMud_Size  index;

    /*
     * TODO: Check arguments.
     */

    result = LMud_Any_FromInteger(1);

    for (index = 0; index < argument_count; index++)
    {
        LMud_Lisp_Mul2(fiber->lisp, result, arguments[index], &result);
    }

    LMud_Fiber_SetAccumulator(fiber, result);
}

void LMud_Builtin_Divide(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any   result;
    LMud_Size  index;

    /*
     * TODO: Check arguments.
     */

    if (argument_count == 0) {
        result = LMud_Any_FromInteger(1);
    } else if (argument_count == 1) {
        result = arguments[0];
    } else {
        result = arguments[0];

        for (index = 1; index < argument_count; index++)
        {
            LMud_Lisp_Div2(fiber->lisp, result, arguments[index], &result);
        }
    }

    LMud_Fiber_SetAccumulator(fiber, result);
}

void LMud_Builtin_Modulo(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any   result;
    LMud_Size  index;

    /*
     * TODO: Check arguments.
     */

    if (argument_count == 0) {
        result = LMud_Any_FromInteger(0);
    } else if (argument_count == 1) {
        result = arguments[0];
    } else {
        result = arguments[0];

        for (index = 1; index < argument_count; index++)
        {
            LMud_Lisp_Mod2(fiber->lisp, result, arguments[index], &result);
        }
    }

    LMud_Fiber_SetAccumulator(fiber, result);
}


void LMud_Lisp_InstallBuiltins(struct LMud_Lisp* lisp)
{
    LMud_Lisp_InstallBuiltin(lisp, "FUNCALL", LMud_Builtin_Funcall);
    LMud_Lisp_InstallBuiltin(lisp, "APPLY", LMud_Builtin_Apply);
    LMud_Lisp_InstallBuiltin(lisp, "VALUES", LMud_Builtin_Values);
    LMud_Lisp_InstallBuiltin(lisp, "SYMBOLP", LMud_Builtin_Symbolp);
    LMud_Lisp_InstallBuiltin(lisp, "GENSYM", LMud_Builtin_Gensym);
    LMud_Lisp_InstallBuiltin(lisp, "SYMBOL-PACKAGE", LMud_Builtin_SymbolPackage);
    LMud_Lisp_InstallBuiltin(lisp, "SYMBOL-NAME", LMud_Builtin_SymbolName);
    LMud_Lisp_InstallBuiltin(lisp, "SYMBOL-VALUE", LMud_Builtin_SymbolValue);
    LMud_Lisp_InstallBuiltin(lisp, "SET-SYMBOL-VALUE", LMud_Builtin_SetSymbolValue);
    LMud_Lisp_InstallBuiltin(lisp, "SYMBOL-FUNCTION", LMud_Builtin_SymbolFunction);
    LMud_Lisp_InstallBuiltin(lisp, "SET-SYMBOL-FUNCTION", LMud_Builtin_SetSymbolFunction);
    LMud_Lisp_InstallBuiltin(lisp, "SYMBOL-MACRO", LMud_Builtin_SymbolMacro);
    LMud_Lisp_InstallBuiltin(lisp, "SET-SYMBOL-MACRO", LMud_Builtin_SetSymbolMacro);
    LMud_Lisp_InstallBuiltin(lisp, "SYMBOL-PLIST", LMud_Builtin_SymbolPlist);
    LMud_Lisp_InstallBuiltin(lisp, "SET-SYMBOL-PLIST", LMud_Builtin_SetSymbolPlist);
    LMud_Lisp_InstallBuiltin(lisp, "PACKAGEP", LMud_Builtin_Packagep);
    LMud_Lisp_InstallBuiltin(lisp, "PACKAGE-NAME", LMud_Builtin_PackageName);
    LMud_Lisp_InstallBuiltin(lisp, "CONSP", LMud_Builtin_Consp);
    LMud_Lisp_InstallBuiltin(lisp, "CONS", LMud_Builtin_Cons);
    LMud_Lisp_InstallBuiltin(lisp, "CAR", LMud_Builtin_Car);
    LMud_Lisp_InstallBuiltin(lisp, "CDR", LMud_Builtin_Cdr);
    LMud_Lisp_InstallBuiltin(lisp, "RPLACA", LMud_Builtin_Rplaca);
    LMud_Lisp_InstallBuiltin(lisp, "RPLACD", LMud_Builtin_Rplacd);
    LMud_Lisp_InstallBuiltin(lisp, "EQ", LMud_Builtin_Eq);
    LMud_Lisp_InstallBuiltin(lisp, "LIST", LMud_Builtin_List);
    LMud_Lisp_InstallBuiltin(lisp, "LIST*", LMud_Builtin_ListStar);
    LMud_Lisp_InstallBuiltin(lisp, "LENGTH", LMud_Builtin_Length);
    LMud_Lisp_InstallBuiltin(lisp, "VECTORP", LMud_Builtin_Vectorp);
    LMud_Lisp_InstallBuiltin(lisp, "VECTOR", LMud_Builtin_Vector);
    LMud_Lisp_InstallBuiltin(lisp, "STRINGP", LMud_Builtin_Stringp);
    LMud_Lisp_InstallBuiltin(lisp, "STRING", LMud_Builtin_String);
    LMud_Lisp_InstallBuiltin(lisp, "AREF", LMud_Builtin_Aref);
    LMud_Lisp_InstallBuiltin(lisp, "CHARACTERP", LMud_Builtin_Characterp);
    LMud_Lisp_InstallBuiltin(lisp, "CHAR-CODE", LMud_Builtin_CharCode);
    LMud_Lisp_InstallBuiltin(lisp, "CODE-CHAR", LMud_Builtin_CodeChar);
    LMud_Lisp_InstallBuiltin(lisp, "NUMERATOR", LMud_Builtin_Numerator);
    LMud_Lisp_InstallBuiltin(lisp, "DENOMINATOR", LMud_Builtin_Denominator);
    LMud_Lisp_InstallBuiltin(lisp, "=", LMud_Builtin_NumericEqual);
    LMud_Lisp_InstallBuiltin(lisp, "<", LMud_Builtin_NumericLess);
    LMud_Lisp_InstallBuiltin(lisp, "<=", LMud_Builtin_NumericLessEqual);
    LMud_Lisp_InstallBuiltin(lisp, ">", LMud_Builtin_NumericGreater);
    LMud_Lisp_InstallBuiltin(lisp, ">=", LMud_Builtin_NumericGreaterEqual);
    LMud_Lisp_InstallBuiltin(lisp, "+", LMud_Builtin_Plus);
    LMud_Lisp_InstallBuiltin(lisp, "-", LMud_Builtin_Minus);
    LMud_Lisp_InstallBuiltin(lisp, "*", LMud_Builtin_Multiply);
    LMud_Lisp_InstallBuiltin(lisp, "/", LMud_Builtin_Divide);
    LMud_Lisp_InstallBuiltin(lisp, "MOD", LMud_Builtin_Modulo);

    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.TEST", "HELLO-WORLD", LMud_Builtin_HelloWorld);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%QUIT", LMud_Builtin_Quit);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%CUSTOMP", LMud_Builtin_Customp);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%MAKE-CUSTOM", LMud_Builtin_MakeCustom);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%CUSTOM-META", LMud_Builtin_CustomMeta);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%CUSTOM-SET-META", LMud_Builtin_CustomSetMeta);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%CUSTOM-SIZE", LMud_Builtin_CustomSize);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%CUSTOM-AT", LMud_Builtin_CustomAt);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%CUSTOM-SET", LMud_Builtin_CustomSet);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "BYTESP", LMud_Builtin_Bytesp);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "BYTES", LMud_Builtin_Bytes);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%ASET", LMud_Builtin_Aset);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%COMPILE", LMud_Builtin_Compile);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%BYTECODE-FUNCTION-P", LMud_Builtin_BytecodeFunctionp);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%FUNCTION-BYTECODES", LMud_Builtin_FunctionBytecodes);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%FUNCTION-CONSTANTS", LMud_Builtin_FunctionConstants);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.DUMMY", "%READ", LMud_Builtin_Read);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.DUMMY", "%PRINC", LMud_Builtin_Princ);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.DUMMY", "%PRIN1", LMud_Builtin_Prin1);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.DUMMY", "%TERPRI", LMud_Builtin_Terpri);
}
