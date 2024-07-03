
#include <lmud/lmud.h>
#include <lmud/net/net.h>
#include <lmud/lisp/gc.h>
#include <lmud/lisp/objects/builtin.h>
#include <lmud/lisp/objects/function.h>
#include <lmud/lisp/runtime/fiber.h>
#include <lmud/lisp/runtime/frame.h>
#include <lmud/lisp/math.h>
#include <lmud/lisp/io.h>
#include <lmud/util/stringbuilder.h>
#include <lmud/util/utf8.h>

#include "builtins.h"


static void LMud_Fiber_CheckArgs_Error(struct LMud_Fiber* fiber, LMud_Any* args, LMud_Size size, LMud_Size min, LMud_Size max, const char* func, const char* file, unsigned int line)
{
    char  buffer[1024];

    (void) args;

    if (max == LMud_VARIADIC_ARGS) {
        snprintf(buffer, sizeof(buffer), "Wrong number of arguments! Builtin: %s(...) in %s:%u got %zu, expected at least %zu.", func, file, line, size, min);
    } else if (max == min) {
        snprintf(buffer, sizeof(buffer), "Wrong number of arguments! Builtin: %s(...) in %s:%u got %zu, expected between exactly %zu.", func, file, line, size, max);
    } else {
        snprintf(buffer, sizeof(buffer), "Wrong number of arguments! Builtin: %s(...) in %s:%u got %zu, expected between %zu and %zu.", func, file, line, size, min, max);
    }

    LMud_Fiber_PerformError(fiber, buffer);
}

bool LMud_Fiber_CheckArgs(struct LMud_Fiber* fiber, LMud_Any* args, LMud_Size size, LMud_Size min, LMud_Size max, const char* func, const char* file, unsigned int line)
{
    (void) args;

    if (size < min || size > max)
    {
        LMud_Fiber_CheckArgs_Error(fiber, args, size, min, max, func, file, line);
        return false;
    }
    return true;
}

#define CHECK_ARGS_BASE(min, max) if (!LMud_Fiber_CheckArgs(fiber, arguments, argument_count, min, max, __func__, __FILE__, __LINE__)) return;
#define CHECK_ARGS(min, max) CHECK_ARGS_BASE(min, max)
#define CHECK_ARGS_VARIADIC(min) CHECK_ARGS(min, LMud_VARIADIC_ARGS)
#define NO_ARGS CHECK_ARGS(0, 0)


void LMud_Builtin_HelloWorld(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    (void) fiber;
    (void) arguments;
    (void) argument_count;

    printf("  Hello, world!\n");
}

void LMud_Builtin_Log(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(2, 2);

    LMud_Logf(fiber->lisp->mud, LMud_Any_AsInteger(arguments[0]), "%s", LMud_String_Chars(LMud_Any_AsPointer(arguments[1])));
}

void LMud_Builtin_Quit(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    NO_ARGS;
    printf("\n<<QUIT BUILTIN WAS TRIGGERED>>\n");
    exit(0);
}

void LMud_Builtin_Apply(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any   function;
    LMud_Any   iterator;
    LMud_Size  extra_argument_count;
    LMud_Size  index;

    CHECK_ARGS_VARIADIC(1);

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

void LMud_Builtin_Funcall(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS_VARIADIC(1);
    LMud_Fiber_Enter(fiber, arguments[0], &arguments[1], argument_count - 1);
}

void LMud_Builtin_FuncallForward(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_Frame*  top;
    LMud_Any            function;
    LMud_Size           forwarded_fixed_arg_count;
    LMud_Size           forwarded_extra_arg_count;
    LMud_Size           forwarded_arg_count;
    LMud_Size           total_new_arg_count;
    LMud_Size           index;
    LMud_Size           fill;

    CHECK_ARGS_VARIADIC(1);

    top                       = fiber->top;
    function                  = arguments[0];

    forwarded_fixed_arg_count = LMud_Frame_FixedArgumentCount(top);
    forwarded_extra_arg_count = LMud_Frame_ExtraArgumentCount(top);
    forwarded_arg_count       = forwarded_fixed_arg_count + forwarded_extra_arg_count;
    total_new_arg_count       = forwarded_arg_count + argument_count - 1;

    LMud_Any new_args[total_new_arg_count];

    fill = 0;

    for (index = 1; index < argument_count; index++)
        new_args[fill++] = arguments[index];
    for (index = 0; index < forwarded_fixed_arg_count; index++)
        new_args[fill++] = *LMud_Frame_FixedArgumentRef(top, index);
    for (index = 0; index < forwarded_extra_arg_count; index++)
        new_args[fill++] = *LMud_Frame_ExtraArgumentRef(top, index);
    
    assert(fill == total_new_arg_count);

    LMud_Fiber_Enter(fiber, function, new_args, total_new_arg_count);
}

void LMud_Builtin_FuncallForwardRest(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_Frame*  top;
    LMud_Any            function;
    LMud_Size           forwarded_extra_arg_count;
    LMud_Size           forwarded_arg_count;
    LMud_Size           total_new_arg_count;
    LMud_Size           index;
    LMud_Size           fill;

    CHECK_ARGS_VARIADIC(1);

    top                       = fiber->top;
    function                  = arguments[0];

    forwarded_extra_arg_count = LMud_Frame_ExtraArgumentCount(top);
    forwarded_arg_count       = forwarded_extra_arg_count;
    total_new_arg_count       = forwarded_arg_count + argument_count - 1;

    LMud_Any new_args[total_new_arg_count];

    fill = 0;

    for (index = 1; index < argument_count; index++)
        new_args[fill++] = arguments[index];
    for (index = 0; index < forwarded_extra_arg_count; index++)
        new_args[fill++] = *LMud_Frame_ExtraArgumentRef(top, index);
    
    assert(fill == total_new_arg_count);

    LMud_Fiber_Enter(fiber, function, new_args, total_new_arg_count);
}

void LMud_Builtin_GivenArgumentCount(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    NO_ARGS;
    LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromInteger(LMud_Frame_GivenArgumentCount(fiber->top)));
}

void LMud_Builtin_GivenArgumentRef(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, *LMud_Frame_GivenArgumentRef(fiber->top, LMud_Any_AsInteger(arguments[0])));
}

void LMud_Builtin_Values(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Fiber_Values(fiber, arguments, argument_count);
}

void LMud_Builtin_GetCustomDispatcherFunction(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    NO_ARGS;
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_CustomDispatcherFunction(fiber->lisp));
}

void LMud_Builtin_SetCustomDispatcherFunction(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Lisp_SetCustomDispatcherFunction(fiber->lisp, arguments[0]);
    LMud_Fiber_SetAccumulator(fiber, arguments[0]);
}

void LMud_Builtin_Symbolp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsSymbol(fiber->lisp, arguments[0])));
}

void LMud_Builtin_Gensymp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsGensym(fiber->lisp, arguments[0])));
}

void LMud_Builtin_FindPackage(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Package(fiber->lisp, arguments[0]));
}

void LMud_Builtin_Intern(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 2);
    if (argument_count == 1)
        LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_InternInPackageLL(fiber->lisp, fiber->lisp->constants.default_package, arguments[0]));
    else
        LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_InternInPackageLL(fiber->lisp, arguments[1], arguments[0]));
}

void LMud_Builtin_AllSymbols(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_Package*  package;
    struct LMud_Symbol*   symbol;
    LMud_Any              list;

    NO_ARGS;

    list = LMud_Lisp_Nil(fiber->lisp);

    for (package = fiber->lisp->objects.packages; package != NULL; package = package->next)
    {
        for (symbol = package->symbols.symbols; symbol != NULL; symbol = symbol->next)
        {
            list = LMud_Lisp_Cons(fiber->lisp, LMud_Any_FromPointer(symbol), list);
        }
    }

    LMud_Fiber_SetAccumulator(fiber, list);
}

void LMud_Builtin_Gensym(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    NO_ARGS;
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Gensym(fiber->lisp));
}

void LMud_Builtin_SymbolPackage(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Symbol_Package(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_SymbolName(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Symbol_Name(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_SymbolValue(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Symbol_Value(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_SetSymbolValue(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(2, 2);
    LMud_Symbol_SetValue(LMud_Any_AsPointer(arguments[0]), arguments[1]);
    LMud_Fiber_SetAccumulator(fiber, arguments[1]);
}

void LMud_Builtin_SymbolFunction(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Symbol_Function(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_SetSymbolFunction(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(2, 2);
    LMud_Symbol_SetFunction(LMud_Any_AsPointer(arguments[0]), arguments[1]);
    LMud_Fiber_SetAccumulator(fiber, arguments[1]);
}

void LMud_Builtin_SymbolMacro(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Symbol_Macro(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_SetSymbolMacro(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(2, 2);
    LMud_Symbol_SetMacro(LMud_Any_AsPointer(arguments[0]), arguments[1]);
    LMud_Fiber_SetAccumulator(fiber, arguments[1]);
}

void LMud_Builtin_SymbolPlist(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Symbol_Plist(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_SetSymbolPlist(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(2, 2);
    LMud_Symbol_SetPlist(LMud_Any_AsPointer(arguments[0]), arguments[1]);
    LMud_Fiber_SetAccumulator(fiber, arguments[1]);
}

void LMud_Builtin_Packagep(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsPackage(fiber->lisp, arguments[0])));
}

void LMud_Builtin_PackageName(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Package_Name(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_Consp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsCons(fiber->lisp, arguments[0])));
}

void LMud_Builtin_Cons(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(2, 2);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Cons(fiber->lisp, arguments[0], arguments[1]));
}

void LMud_Builtin_Car(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Car(fiber->lisp, arguments[0]));
}

void LMud_Builtin_Cdr(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Cdr(fiber->lisp, arguments[0]));
}

void LMud_Builtin_Rplaca(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(2, 2);
    LMud_Lisp_Rplaca(fiber->lisp, arguments[0], arguments[1]);
    LMud_Fiber_SetAccumulator(fiber, arguments[1]);
}

void LMud_Builtin_Rplacd(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(2, 2);
    LMud_Lisp_Rplacd(fiber->lisp, arguments[0], arguments[1]);
    LMud_Fiber_SetAccumulator(fiber, arguments[1]);
}

void LMud_Builtin_Customp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsCustom(fiber->lisp, arguments[0])));
}

void LMud_Builtin_MakeCustom(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, LMud_VARIADIC_ARGS);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Custom(fiber->lisp, arguments[0], &arguments[1], argument_count - 1));
}

void LMud_Builtin_CustomMeta(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Custom_Meta(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_CustomSetMeta(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(2, 2);
    LMud_Custom_SetMeta(LMud_Any_AsPointer(arguments[0]), arguments[1]);
    LMud_Fiber_SetAccumulator(fiber, arguments[1]);
}

void LMud_Builtin_CustomSize(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromInteger(LMud_Custom_Size(LMud_Any_AsPointer(arguments[0]))));
}

void LMud_Builtin_CustomAt(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(2, 2);
    LMud_Fiber_SetAccumulator(fiber, LMud_Custom_At(LMud_Any_AsPointer(arguments[0]), LMud_Any_AsInteger(arguments[1])));
}

void LMud_Builtin_CustomSet(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(3, 3);
    LMud_Custom_Set(LMud_Any_AsPointer(arguments[0]), LMud_Any_AsInteger(arguments[1]), arguments[2]);
    LMud_Fiber_SetAccumulator(fiber, arguments[2]);
}

void LMud_Builtin_Eq(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Size  index;

    CHECK_ARGS(2, LMud_VARIADIC_ARGS);

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

    CHECK_ARGS(1, 1);

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
    CHECK_ARGS(1, 1);
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
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsArray(fiber->lisp, arguments[0])));
}

void LMud_Builtin_Vector(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_MakeArray_FromData(fiber->lisp, argument_count, arguments));
}

void LMud_Builtin_Stringp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsString(fiber->lisp, arguments[0])));
}

void LMud_Builtin_String(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_StringBuilder  builder;
    struct LMud_Utf8_Encoder   encoder;
    LMud_Any                   result;
    LMud_Size                  index;

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

    CHECK_ARGS(2, 2);
    LMud_Lisp_Aref(fiber->lisp, arguments[0], arguments[1], &result);
    LMud_Fiber_SetAccumulator(fiber, result);
}

void LMud_Builtin_Aset(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(3, 3);
    LMud_Lisp_Aset(fiber->lisp, arguments[0], arguments[1], arguments[2]);
    LMud_Fiber_SetAccumulator(fiber, arguments[2]);
}

void LMud_Builtin_MachineFunctionp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsBuiltin(fiber->lisp, arguments[0])));
}

void LMud_Builtin_MachineFunctionName(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_String(fiber->lisp, LMud_Builtin_GetName(LMud_Any_AsPointer(arguments[0]))));
}

void LMud_Builtin_Closurep(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsClosure(fiber->lisp, arguments[0])));
}

void LMud_Builtin_BytecodeFunctionp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsFunction(fiber->lisp, arguments[0])));
}

void LMud_Builtin_FunctionBytecodes(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Function_Bytecodes(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_FunctionConstants(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Function_Constants(LMud_Any_AsPointer(arguments[0])));
}

void LMud_Builtin_FunctionSourceCode(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any  result;

    CHECK_ARGS(1, 1);
    result = LMud_Function_SourceCode(LMud_Any_AsPointer(arguments[0]));
    LMud_Fiber_SetAccumulator(fiber, result);
}

void LMud_Builtin_Compile(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any  result;

    /*
     * TODO: Check whether the compilation was successful.
     */
    
    CHECK_ARGS(1, 1);

    LMud_Lisp_Compile(fiber->lisp, arguments[0], &result);

    LMud_Fiber_SetAccumulator(fiber, result);
}


void LMud_Builtin_Read(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any  result;

    NO_ARGS;

    if (!LMud_Lisp_Read(fiber->lisp, &fiber->lisp->standard_input, &result))
        result = LMud_Lisp_Nil(fiber->lisp);

    LMud_Fiber_SetAccumulator(fiber, result);
}

void LMud_Builtin_Princ(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Lisp_Print(fiber->lisp, arguments[0], stdout, false);
    LMud_Fiber_SetAccumulator(fiber, arguments[0]);
}

void LMud_Builtin_Prin1(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Lisp_Print(fiber->lisp, arguments[0], stdout, true);
    LMud_Fiber_SetAccumulator(fiber, arguments[0]);
}

void LMud_Builtin_Terpri(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    NO_ARGS;
    printf("\n");
}


void LMud_Builtin_Characterp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Any_IsCharacter(arguments[0])));
}

void LMud_Builtin_CharCode(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromInteger(LMud_Rune_AsInteger(LMud_Any_AsCharacter(arguments[0]))));
}

void LMud_Builtin_CodeChar(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromCharacter(LMud_Rune_FromInteger(LMud_Any_AsInteger(arguments[0]))));
}

void LMud_Builtin_CharUpcase(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromCharacter(LMud_Rune_UpperCase(LMud_Any_AsCharacter(arguments[0]))));
}


void LMud_Builtin_Integerp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Any_IsInteger(arguments[0])));
}

void LMud_Builtin_Ratiop(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsRatio(fiber->lisp, arguments[0])));
}

void LMud_Builtin_Numerator(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any  result;

    CHECK_ARGS(1, 1);
    result = LMud_Lisp_Numerator(fiber->lisp, arguments[0]);
    LMud_Fiber_SetAccumulator(fiber, result);
}

void LMud_Builtin_Denominator(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any  result;

    CHECK_ARGS(1, 1);
    result = LMud_Lisp_Denominator(fiber->lisp, arguments[0]);
    LMud_Fiber_SetAccumulator(fiber, result);
}

static void LMud_Builtin_NumericComparison(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count, bool (*comparison)(struct LMud_Lisp*, LMud_Any, LMud_Any))
{
    LMud_Any   value;
    LMud_Size  index;

    CHECK_ARGS(2, LMud_VARIADIC_ARGS);
    
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

    CHECK_ARGS(1, LMud_VARIADIC_ARGS);

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

    CHECK_ARGS(1, LMud_VARIADIC_ARGS);

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

    CHECK_ARGS(2, LMud_VARIADIC_ARGS);

    result = arguments[0];

    for (index = 1; index < argument_count; index++)
    {
        LMud_Lisp_Mod2(fiber->lisp, result, arguments[index], &result);
    }

    LMud_Fiber_SetAccumulator(fiber, result);
}

void LMud_Builtin_Truncate(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any  result;

    CHECK_ARGS(1, 2);

    if (argument_count == 1) {
        LMud_Lisp_Truncate(fiber->lisp,
                           LMud_Lisp_Numerator(fiber->lisp, arguments[0]),
                           LMud_Lisp_Denominator(fiber->lisp, arguments[0]),
                           &result);
    } else if (argument_count == 2) {
        LMud_Lisp_Truncate(fiber->lisp, arguments[0], arguments[1], &result);
    } else {
        result = LMud_Any_FromInteger(0);
    }
    
    LMud_Fiber_SetAccumulator(fiber, result);
}

void LMud_Builtin_LogAnd(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any   result;
    LMud_Size  index;

    /*
     * TODO: Check arguments.
     */

    result = LMud_Any_FromInteger(-1);

    for (index = 0; index < argument_count; index++)
    {
        LMud_Lisp_LogAnd2(fiber->lisp, result, arguments[index], &result);
    }

    LMud_Fiber_SetAccumulator(fiber, result);
}

void LMud_Builtin_LogIor(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any   result;
    LMud_Size  index;

    /*
     * TODO: Check arguments.
     */

    result = LMud_Any_FromInteger(0);

    for (index = 0; index < argument_count; index++)
    {
        LMud_Lisp_LogIor2(fiber->lisp, result, arguments[index], &result);
    }

    LMud_Fiber_SetAccumulator(fiber, result);
}

void LMud_Builtin_LogXor(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any   result;
    LMud_Size  index;

    /*
     * TODO: Check arguments.
     */

    result = LMud_Any_FromInteger(0);

    for (index = 0; index < argument_count; index++)
    {
        LMud_Lisp_LogXor2(fiber->lisp, result, arguments[index], &result);
    }
}

void LMud_Builtin_LogNot(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any  result;

    /*
     * TODO: Check arguments.
     */
    (void) argument_count;

    LMud_Lisp_LogNot(fiber->lisp, arguments[0], &result);

    LMud_Fiber_SetAccumulator(fiber, result);
}

void LMud_Builtin_Ash(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any  result;

    /*
     * TODO: Check arguments.
     */
    (void) argument_count;

    LMud_Lisp_Ash(fiber->lisp, arguments[0], arguments[1], &result);

    LMud_Fiber_SetAccumulator(fiber, result);
}

void LMud_Builtin_GarbageCollect(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_GCStats  stats;
    
    NO_ARGS;
    
    /*
     * TODO, FIXME, XXX: Will this invalidate our arguments?
     */
    LMud_Lisp_GarbageCollect(fiber->lisp, &stats);

    LMud_Any  values[2] = {
        LMud_Any_FromInteger(stats.objects_kept),
        LMud_Any_FromInteger(stats.objects_freed)
    };

    LMud_Fiber_Values(fiber, values, sizeof(values) / sizeof(values[0]));
}

void LMud_Builtin_KickstartTask(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Lisp_Kickstart(fiber->lisp, arguments[0]);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Nil(fiber->lisp));
}

void LMud_Builtin_Random(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromInteger(rand() % LMud_Any_AsInteger(arguments[0])));
}

void LMud_Builtin_Signal(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Fiber_Values(fiber, arguments, argument_count);
    LMud_Fiber_SignalAndUnwind(fiber);
}

void LMud_Builtin_OnConnect(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Lisp_SetNewConnectionFunction(fiber->lisp, arguments[0]);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Nil(fiber->lisp));
}

void LMud_Builtin_OpenV4(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(4, 4);

    if (LMud_Net_OpenV4(&fiber->lisp->mud->net, LMud_String_Chars(LMud_Any_AsPointer(arguments[3])), LMud_String_Chars(LMud_Any_AsPointer(arguments[0])), LMud_Any_AsInteger(arguments[1]), arguments[2])) {
        LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_T(fiber->lisp));
    } else {
        LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Nil(fiber->lisp));
    }
}

void LMud_Builtin_OpenV6(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(4, 4);

    if (LMud_Net_OpenV6(&fiber->lisp->mud->net, LMud_String_Chars(LMud_Any_AsPointer(arguments[3])), LMud_String_Chars(LMud_Any_AsPointer(arguments[0])), LMud_Any_AsInteger(arguments[1]), arguments[2])) {
        LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_T(fiber->lisp));
    } else {
        LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Nil(fiber->lisp));
    }
}

void LMud_Builtin_Portp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    /*
     * TODO: Check arguments.
     */
    (void) argument_count;
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsPort(fiber->lisp, arguments[0])));
}

void LMud_Builtin_OpenFd(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_Connection*  connection;

    CHECK_ARGS(1, 1);

    if (LMud_Net_RegisterClientFileDescriptor(&fiber->lisp->mud->net, LMud_Any_AsInteger(arguments[0]), false, &connection)) {
        LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Port(fiber->lisp, connection));
    } else {
        LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Nil(fiber->lisp));
    }
}

void LMud_Builtin_OpenFile(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_Connection*  connection;
    int                      fd;

    CHECK_ARGS(1, 1);

    fd = open(LMud_String_Chars(LMud_Any_AsPointer(arguments[0])), O_RDWR);

    if (fd >= 0 && LMud_Net_RegisterClientFileDescriptor(&fiber->lisp->mud->net, fd, true, &connection)) {
        LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Port(fiber->lisp, connection));
    } else {
        LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Nil(fiber->lisp));
    }
}

void LMud_Builtin_Close(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Port_Close(LMud_Any_AsPointer(arguments[0]));
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Nil(fiber->lisp));
}

void LMud_Builtin_PortEof(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Port_Eof(LMud_Any_AsPointer(arguments[0]), fiber);
}

void LMud_Builtin_PortReadByte(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Port_FiberReadByte(LMud_Any_AsPointer(arguments[0]), fiber);
}

void LMud_Builtin_PortWriteByte(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    bool  success;

    CHECK_ARGS(2, 2);
    success = LMud_Port_WriteByte(LMud_Any_AsPointer(arguments[0]), LMud_Any_AsInteger(arguments[1]));
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, success));
}

void LMud_Builtin_PortUnreadByte(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(2, 2);
    LMud_Port_PushbackByte(LMud_Any_AsPointer(arguments[0]), LMud_Any_AsInteger(arguments[1]));
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Nil(fiber->lisp));
}

void LMud_Builtin_PortUnreadCharacter(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(2, 2);
    LMud_Port_PushbackRune(LMud_Any_AsPointer(arguments[0]), LMud_Any_AsCharacter(arguments[1]));
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Nil(fiber->lisp));
}

void LMud_Builtin_CurrentPort(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(0, 0);
    LMud_Fiber_SetAccumulator(fiber, LMud_Fiber_GetPort(fiber));
}

void LMud_Builtin_SetCurrentPort(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetPort(fiber, arguments[0]);
    LMud_Fiber_SetAccumulator(fiber, arguments[0]);
}

void LMud_Builtin_Processp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsProcess(fiber->lisp, arguments[0])));
}

void LMud_Builtin_CurrentProcess(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(0, 0);
    LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromPointer(LMud_Fiber_GetProcess(fiber)));
}

void LMud_Builtin_ProcessState(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Process_GetStateAsLispValue(LMud_Any_AsPointer(arguments[0]), fiber->lisp));
}

void LMud_Builtin_CreateProcess(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, LMud_VARIADIC_ARGS);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_KickstartProcess(fiber->lisp, arguments[0], arguments + 1, argument_count - 1));
}

void LMud_Builtin_WaitForProcess(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Process_FiberWait(LMud_Any_AsPointer(arguments[0]), fiber);
}

void LMud_Builtin_AllProcesses(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    LMud_Any            list;
    struct LMud_Fiber*  iterator;

    NO_ARGS;

    list = LMud_Lisp_Nil(fiber->lisp);

    for (iterator = LMud_Scheduler_GetAllFibers_UNSAFE(LMud_Lisp_Scheduler(fiber->lisp)); iterator != NULL; iterator = iterator->next)
    {
        list = LMud_Lisp_Cons(fiber->lisp, LMud_Any_FromPointer(LMud_Fiber_GetProcess(iterator)), list);
    }

    LMud_Fiber_SetAccumulator(fiber, list);
}

void LMud_Builtin_StackFrameP(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    CHECK_ARGS(1, 1);
    LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Boolean(fiber->lisp, LMud_Lisp_IsStackFrame(fiber->lisp, arguments[0])));
}

void LMud_Builtin_ProcessStackFrames(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_Process*  process;
    struct LMud_Fiber*    process_fiber;

    CHECK_ARGS(1, 1);

    if (!LMud_Lisp_IsProcess(fiber->lisp, arguments[0])) {
        LMud_Fiber_PerformError(fiber, "Expected a process.");
        return;
    } else {
        process       = LMud_Any_AsPointer(arguments[0]);
        process_fiber = LMud_Process_GetFiber(process);

        if (process_fiber->top == NULL) {
            LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Nil(fiber->lisp));
        } else {
            LMud_Fiber_SetAccumulator(fiber, LMud_Frame_GetLispStackFrame(process_fiber->top, fiber->lisp));
        }
    }
}

void LMud_Builtin_StackFramePrevious(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_StackFrame*  frame;
    struct LMud_Frame*       prev;

    CHECK_ARGS(1, 1);

    if (!LMud_Lisp_IsStackFrame(fiber->lisp, arguments[0])) {
        LMud_Fiber_PerformError(fiber, "Expected a stack frame.");
        return;
    } else {
        frame = LMud_Any_AsPointer(arguments[0]);
        prev  = LMud_Frame_GetParent(LMud_StackFrame_GetFrame(frame));
        if (prev == NULL) {
            LMud_Fiber_SetAccumulator(fiber, LMud_Lisp_Nil(fiber->lisp));
        } else {
            LMud_Fiber_SetAccumulator(fiber, LMud_Frame_GetLispStackFrame(prev, fiber->lisp));
        }
    }
}

void LMud_Builtin_StackFrameLexical(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_StackFrame*  frame;

    CHECK_ARGS(1, 1);

    if (!LMud_Lisp_IsStackFrame(fiber->lisp, arguments[0])) {
        LMud_Fiber_PerformError(fiber, "Expected a stack frame.");
        return;
    } else {
        frame = LMud_Any_AsPointer(arguments[0]);
        LMud_Fiber_SetAccumulator(fiber, LMud_Frame_GetLispStackFrame(LMud_Frame_GetParent(LMud_StackFrame_GetFrame(frame)), fiber->lisp));
    }
}

void LMud_Builtin_StackFrameFunction(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_StackFrame*  frame;

    CHECK_ARGS(1, 1);

    if (!LMud_Lisp_IsStackFrame(fiber->lisp, arguments[0])) {
        LMud_Fiber_PerformError(fiber, "Expected a stack frame.");
        return;
    } else {
        frame = LMud_Any_AsPointer(arguments[0]);
        LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromPointer(LMud_Frame_GetFunction(LMud_StackFrame_GetFrame(frame))));
    }
}

void LMud_Builtin_StackFrameIp(struct LMud_Fiber* fiber, LMud_Any* arguments, LMud_Size argument_count)
{
    struct LMud_StackFrame*  frame;

    CHECK_ARGS(1, 1);

    if (!LMud_Lisp_IsStackFrame(fiber->lisp, arguments[0])) {
        LMud_Fiber_PerformError(fiber, "Expected a stack frame.");
        return;
    } else {
        frame = LMud_Any_AsPointer(arguments[0]);
        LMud_Fiber_SetAccumulator(fiber, LMud_Any_FromInteger(LMud_Frame_GetIP(LMud_StackFrame_GetFrame(frame))));
    }
}


void LMud_Lisp_InstallBuiltins(struct LMud_Lisp* lisp)
{
    LMud_Lisp_InstallBuiltin(lisp, "FUNCALL", LMud_Builtin_Funcall);
    LMud_Lisp_InstallBuiltin(lisp, "APPLY", LMud_Builtin_Apply);
    LMud_Lisp_InstallBuiltin(lisp, "VALUES", LMud_Builtin_Values);
    LMud_Lisp_InstallBuiltin(lisp, "SYMBOLP", LMud_Builtin_Symbolp);
    LMud_Lisp_InstallBuiltin(lisp, "INTERN", LMud_Builtin_Intern);
    LMud_Lisp_InstallBuiltin(lisp, "FIND-PACKAGE", LMud_Builtin_FindPackage);
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
    LMud_Lisp_InstallBuiltin(lisp, "CHAR-UPCASE", LMud_Builtin_CharUpcase);
    LMud_Lisp_InstallBuiltin(lisp, "INTEGERP", LMud_Builtin_Integerp);
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
    LMud_Lisp_InstallBuiltin(lisp, "TRUNCATE", LMud_Builtin_Truncate);
    LMud_Lisp_InstallBuiltin(lisp, "LOGAND", LMud_Builtin_LogAnd);
    LMud_Lisp_InstallBuiltin(lisp, "LOGIOR", LMud_Builtin_LogIor);
    LMud_Lisp_InstallBuiltin(lisp, "LOGXOR", LMud_Builtin_LogXor);
    LMud_Lisp_InstallBuiltin(lisp, "LOGNOT", LMud_Builtin_LogNot);
    LMud_Lisp_InstallBuiltin(lisp, "ASH", LMud_Builtin_Ash);
    LMud_Lisp_InstallBuiltin(lisp, "RANDOM", LMud_Builtin_Random);

    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.TEST", "HELLO-WORLD", LMud_Builtin_HelloWorld);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "LOG", LMud_Builtin_Log);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%QUIT", LMud_Builtin_Quit);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "FUNCALL-FORWARD", LMud_Builtin_FuncallForward);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "FUNCALL-FORWARD-REST", LMud_Builtin_FuncallForwardRest);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%GIVEN-ARGUMENT-COUNT", LMud_Builtin_GivenArgumentCount);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%GIVEN-ARGUMENT-REF", LMud_Builtin_GivenArgumentRef);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "GENSYMP", LMud_Builtin_Gensymp);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%CUSTOM-DISPATCHER-FUNCTION", LMud_Builtin_GetCustomDispatcherFunction);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "%SET-CUSTOM-DISPATCHER-FUNCTION", LMud_Builtin_SetCustomDispatcherFunction);
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
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "MACHINE-FUNCTION-P", LMud_Builtin_MachineFunctionp);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "MACHINE-FUNCTION-NAME", LMud_Builtin_MachineFunctionName);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "CLOSUREP", LMud_Builtin_Closurep);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "BYTECODE-FUNCTION-P", LMud_Builtin_BytecodeFunctionp);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "FUNCTION-BYTECODES", LMud_Builtin_FunctionBytecodes);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "FUNCTION-CONSTANTS", LMud_Builtin_FunctionConstants);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "FUNCTION-SOURCE-CODE", LMud_Builtin_FunctionSourceCode);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "GARBAGE-COLLECT", LMud_Builtin_GarbageCollect);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "KICKSTART-TASK", LMud_Builtin_KickstartTask);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "SIGNAL", LMud_Builtin_Signal);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "OPEN-V4", LMud_Builtin_OpenV4);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "OPEN-V6", LMud_Builtin_OpenV6);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "PORTP", LMud_Builtin_Portp);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "CLOSE-PORT", LMud_Builtin_Close);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "PORT-EOF-P", LMud_Builtin_PortEof);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "PORT-READ-BYTE", LMud_Builtin_PortReadByte);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "PORT-WRITE-BYTE", LMud_Builtin_PortWriteByte);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "PORT-UNREAD-BYTE", LMud_Builtin_PortUnreadByte);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "PORT-UNREAD-CHAR", LMud_Builtin_PortUnreadCharacter);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "CURRENT-PORT", LMud_Builtin_CurrentPort);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "SET-CURRENT-PORT", LMud_Builtin_SetCurrentPort);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "OPEN-FD", LMud_Builtin_OpenFd);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "OPEN-FILE", LMud_Builtin_OpenFile);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "RATIOP", LMud_Builtin_Ratiop);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "ALL-SYMBOLS", LMud_Builtin_AllSymbols);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "PROCESSP", LMud_Builtin_Processp);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "CURRENT-PROCESS", LMud_Builtin_CurrentProcess);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "PROCESS-STATE", LMud_Builtin_ProcessState);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "CREATE-PROCESS", LMud_Builtin_CreateProcess);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "WAIT-FOR-PROCESS", LMud_Builtin_WaitForProcess);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "ALL-PROCESSES", LMud_Builtin_AllProcesses);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "STACK-FRAME-P", LMud_Builtin_StackFrameP);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "PROCESS-STACK-FRAMES", LMud_Builtin_ProcessStackFrames);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "STACK-FRAME-PREVIOUS", LMud_Builtin_StackFramePrevious);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "STACK-FRAME-LEXICAL", LMud_Builtin_StackFrameLexical);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "STACK-FRAME-FUNCTION", LMud_Builtin_StackFrameFunction);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.INT", "STACK-FRAME-IP", LMud_Builtin_StackFrameIp);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.DUMMY", "%READ", LMud_Builtin_Read);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.DUMMY", "%PRINC", LMud_Builtin_Princ);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.DUMMY", "%PRIN1", LMud_Builtin_Prin1);
    LMud_Lisp_InstallPackagedBuiltin(lisp, "LMUD.DUMMY", "%TERPRI", LMud_Builtin_Terpri);
}
