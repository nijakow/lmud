
#include <lmud/lisp/lisp.h>
#include <lmud/util/memory.h>
#include <lmud/util/stringbuilder.h>
#include <lmud/util/utf8.h>

#include "io.h"


void LMud_Lisp_PrintList(struct LMud_Lisp* lisp, LMud_Any list, FILE* stream, bool escaped)
{
    fprintf(stream, "(");

    LMud_Lisp_Print(lisp, LMud_Lisp_Car(lisp, list), stream, escaped);
    list = LMud_Lisp_Cdr(lisp, list);

    while (LMud_Lisp_IsCons(lisp, list))
    {
        fprintf(stream, " ");
        LMud_Lisp_Print(lisp, LMud_Lisp_Car(lisp, list), stream, escaped);
        list = LMud_Lisp_Cdr(lisp, list);
    }

    if (!LMud_Lisp_IsNil(lisp, list)) {
        fprintf(stream, " . ");
        LMud_Lisp_Print(lisp, list, stream, escaped);
    }

    fprintf(stream, ")");
}

void LMud_Lisp_PrintArray(struct LMud_Lisp* lisp, struct LMud_Array* array, FILE* stream, bool escaped)
{
    LMud_Any   default_value;
    LMud_Size  index;
    LMud_Size  size;

    fprintf(stream, "〈");

    default_value = LMud_Lisp_Nil(lisp);
    size          = LMud_Array_GetSize(array);
    
    for (index = 0; index < size; index++)
    {
        if (index > 0)
            fprintf(stream, " ");
        LMud_Lisp_Print(lisp, LMud_Array_Aref(array, index, default_value), stream, escaped);
    }

    fprintf(stream, "〉");
}

void LMud_Lisp_PrintBytes(struct LMud_Lisp* lisp, struct LMud_Bytes* bytes, FILE* stream)
{
    LMud_Size  index;
    LMud_Size  size;

    (void) lisp;

    fprintf(stream, "⦑");

    size = LMud_Bytes_GetSize(bytes);

    for (index = 0; index < size; index++)
    {
        if (index > 0)
            fprintf(stream, " ");
        fprintf(stream, "%02x", (unsigned char) LMud_Bytes_At(bytes, index));
    }

    fprintf(stream, "⦒");
}

void LMud_Lisp_PrintCharacter(struct LMud_Lisp* lisp, LMud_Any object, FILE* stream, bool escaped)
{
    struct LMud_Utf8_Encoder  encoder;
    const char*               generic_name;
    LMud_Rune                 rune;

    (void) lisp;

    rune = LMud_Any_AsCharacter(object);

    if (escaped) {
        generic_name = LMud_Rune_Name(rune);

        if (generic_name != NULL)
            fprintf(stream, "#\\%s", generic_name);
        else if (!LMud_Rune_IsPrintable(rune))
            fprintf(stream, "#\\U+%04X", rune);
        else {
            LMud_Utf8_Encoder_Create(&encoder, rune);
            fprintf(stream, "#\\%s", LMud_Utf8_Encoder_AsString(&encoder));
            LMud_Utf8_Encoder_Destroy(&encoder);
        }
    } else {
        LMud_Utf8_Encoder_Create(&encoder, rune);
        fprintf(stream, "%s", LMud_Utf8_Encoder_AsString(&encoder));
        LMud_Utf8_Encoder_Destroy(&encoder);
    }
}

bool LMud_Lisp_IsQuoteClause(struct LMud_Lisp* lisp, LMud_Any object)
{
    return LMud_Lisp_IsCons(lisp, object)
        && LMud_Lisp_IsSymbol(lisp, LMud_Lisp_Car(lisp, object))
        && LMud_Any_Eq(LMud_Lisp_Car(lisp, object), lisp->constants.quote);
}

void LMud_Lisp_BeginPrintList(struct LMud_Lisp* lisp, LMud_Any list, FILE* stream, bool escaped)
{
    if (LMud_Lisp_IsQuoteClause(lisp, list)) {
        fprintf(stream, "'");
        LMud_Lisp_Print(lisp, LMud_Lisp_Cadr(lisp, list), stream, escaped);
    } else {
        LMud_Lisp_PrintList(lisp, list, stream, escaped);
    }
}

void LMud_Lisp_PrintSymbol(struct LMud_Lisp* lisp, LMud_Any symbol, FILE* stream, bool escaped)
{
    LMud_Any             package;
    struct LMud_Symbol*  symbol_object;

    (void) escaped;

    symbol_object = LMud_Any_AsPointer(symbol);
    package       = LMud_Symbol_Package(symbol_object);

    if (LMud_Symbol_IsGensym(symbol_object)) {
        fprintf(stream, "⦍GENSYM %p⦎", symbol_object);
    } else {
        if (LMud_Lisp_IsPackage(lisp, package)) {
            if (LMud_Any_Eq(package, lisp->constants.default_package)) {
                // Do nothing
            } else if (LMud_Any_Eq(package, lisp->constants.keyword_package)) {
                fprintf(stream, ":");
            } else {
                LMud_Lisp_Print(lisp, LMud_Package_Name(LMud_Any_AsPointer(package)), stream, false);
                fprintf(stream, "::");
            }
        }
        fprintf(stream, "%s", LMud_Symbol_NameChars(symbol_object));
    }
}

void LMud_Lisp_Print(struct LMud_Lisp* lisp, LMud_Any object, FILE* stream, bool escaped)
{
    void*  pointer;

    if (LMud_Any_IsInteger(object)) {
        fprintf(stream, "%d", LMud_Any_AsInteger(object));
    } else if (LMud_Any_IsCharacter(object)) {
        LMud_Lisp_PrintCharacter(lisp, object, stream, escaped);
    } else if (LMud_Any_IsPointer(object)) {
        pointer = LMud_Any_AsPointer(object);

        if (LMud_Lisp_IsConsPointer(lisp, pointer)) {
            LMud_Lisp_BeginPrintList(lisp, object, stream, escaped);
        } else if (LMud_Lisp_IsArrayPointer(lisp, pointer)) {
            LMud_Lisp_PrintArray(lisp, pointer, stream, escaped);
        } else if (LMud_Lisp_IsBytesPointer(lisp, pointer)) {
            LMud_Lisp_PrintBytes(lisp, pointer, stream);
        } else if (LMud_Lisp_IsStringPointer(lisp, pointer)) {
            if (escaped) {
                fprintf(stream, "\"%s\"", ((struct LMud_String*) pointer)->chars);
            } else {
                fprintf(stream, "%s", ((struct LMud_String*) pointer)->chars);
            }
        } else if (LMud_Lisp_IsSymbolPointer(lisp, pointer)) {
            LMud_Lisp_PrintSymbol(lisp, object, stream, escaped);
        } else if (LMud_Lisp_IsFunctionPointer(lisp, pointer)) {
            fprintf(stream, "⦍BYTE-COMPILED-FUNCTION %p :BYTECODES ", pointer);
            LMud_Lisp_Print(lisp, LMud_Function_Bytecodes(pointer), stream, true);
            fprintf(stream, " :CONSTANTS ");
            LMud_Lisp_Print(lisp, LMud_Function_Constants(pointer), stream, true);
            fprintf(
                stream,
                " :STACK-SIZE %lu :REGISTER-COUNT %lu :FIXED-ARGUMENT-COUNT %lu :LEXICALIZED %s :VARIADIC %s⦎",
                ((struct LMud_Function*) pointer)->info.stack_size,
                ((struct LMud_Function*) pointer)->info.register_count,
                ((struct LMud_Function*) pointer)->info.fixed_argument_count,
                ((struct LMud_Function*) pointer)->info.lexicalized ? "T" : "NIL",
                ((struct LMud_Function*) pointer)->info.variadic ? "T" : "NIL"
            );
        } else if (LMud_Lisp_IsClosurePointer(lisp, pointer)) {
            fprintf(stream, "⦍CLOSURE %p⦎", pointer);
        } else if (LMud_Lisp_IsBuiltinPointer(lisp, pointer)) {
            fprintf(stream, "⦍MACHINE-CODE-FUNCTION :NAME %s⦎", ((struct LMud_Builtin*) pointer)->name);
        } else if (LMud_Lisp_IsRatioPointer(lisp, pointer)) {
            LMud_Lisp_Print(lisp, LMud_Ratio_Numerator((struct LMud_Ratio*) pointer), stream, escaped);
            fprintf(stream, "/");
            LMud_Lisp_Print(lisp, LMud_Ratio_Denominator((struct LMud_Ratio*) pointer), stream, escaped);
        } else if (LMud_Lisp_IsPackagePointer(lisp, pointer)) {
            fprintf(stream, "⦍PACKAGE :NAME ");
            LMud_Lisp_Print(lisp, LMud_Package_Name((struct LMud_Package*) pointer), stream, true);
            fprintf(stream, "⦎");
        } else if (LMud_Lisp_IsCustomPointer(lisp, pointer)) {
            fprintf(stream, "⦍CUSTOM %p⦎", pointer);
        } else {
            fprintf(stream, "⦍UNKNOWN %p⦎", pointer);
        }
    } else {
        fprintf(stream, "⦍UNKNOWN⦎");
    }
}


bool LMud_Lisp_Read_IsNotNewline(char c)
{
    return (c != '\n');
}

bool LMud_Lisp_Read_IsWhitespace(char c)
{
    return (c == ' ')
        || (c == '\t')
        || (c == '\n')
        || (c == '\r');
}

bool LMud_Lisp_Read_IsBreakingChar(char c)
{
    return LMud_Lisp_Read_IsWhitespace(c)
        || (c == '(')
        || (c == ')');
}


bool LMud_Lisp_ReadList(struct LMud_Lisp* lisp, struct LMud_InputStream* stream, LMud_Any* result)
{
    LMud_Any  value;
    LMud_Any  rest;

    LMud_InputStream_SkipIf(stream, &LMud_Lisp_Read_IsWhitespace);

    if (LMud_InputStream_CheckStr(stream, ". ")) {
         if (!(LMud_Lisp_Read(lisp, stream, &value) && LMud_InputStream_CheckStr(stream, ")"))) {
            return false;
         }
    } else if (LMud_InputStream_CheckStr(stream, ")")) {
        value = LMud_Lisp_Nil(lisp);
    } else {
        if (!(LMud_Lisp_Read(lisp, stream, &value) && LMud_Lisp_ReadList(lisp, stream, &rest)))
            return false;
        value = LMud_Lisp_Cons(
            lisp,
            value,
            rest
        );
    }

    *result = value;

    return true;
}


bool LMud_Lisp_ParseInt(struct LMud_Lisp* lisp, const char* buffer, LMud_Any* value)
{
    char*  end;

    (void) lisp;

    *value = LMud_Any_FromInteger((int) strtol(buffer, &end, 10));

    return (*end == '\0');
}


void LMud_Lisp_ReadEscaped(struct LMud_Lisp* lisp, struct LMud_InputStream* stream, struct LMud_StringBuilder* builder, const char* terminator)
{
    (void) lisp;

    while (!LMud_InputStream_Eof(stream))
    {
        if (LMud_InputStream_CheckStr(stream, terminator)) {
            break;
        }

        LMud_StringBuilder_AppendChar(builder, LMud_InputStream_Read(stream));
    }
}

bool LMud_Lisp_ReadString(struct LMud_Lisp* lisp, struct LMud_InputStream* stream, LMud_Any* result)
{
    struct LMud_StringBuilder  builder;
    LMud_Any                   value;

    LMud_StringBuilder_Create(&builder);
    LMud_Lisp_ReadEscaped(lisp, stream, &builder, "\"");
    value = LMud_Lisp_String(lisp, LMud_StringBuilder_GetStatic(&builder));
    LMud_StringBuilder_Destroy(&builder);

    *result = value;

    return true;
}

void LMud_Lisp_ReadUntilBreakingChar(struct LMud_Lisp* lisp, struct LMud_InputStream* stream, struct LMud_StringBuilder* builder)
{
    (void) lisp;

    while (!LMud_InputStream_Eof(stream))
    {
        if (LMud_Lisp_Read_IsBreakingChar(LMud_InputStream_Get(stream)))
            break;

        LMud_StringBuilder_AppendChar(builder, LMud_InputStream_Read(stream));
    }
}

bool LMud_Lisp_ReadAtom(struct LMud_Lisp* lisp, struct LMud_InputStream* stream, LMud_Any* result)
{
    struct LMud_StringBuilder  builder;
    struct LMud_StringBuilder  builder2;
    const char*                ptr;
    const char*                end_first_half;
    const char*                second_half;
    LMud_Any                   package;
    LMud_Any                   value;

    LMud_StringBuilder_Create(&builder);
    {
        LMud_Lisp_ReadUntilBreakingChar(lisp, stream, &builder);
        ptr = (char*) LMud_StringBuilder_GetStatic(&builder);

        if (LMud_Lisp_ParseInt(lisp, ptr, &value)) {
            *result = value;
        } else {
            if (LMud_CStr_FindAndPartition(ptr, "::", &end_first_half, &second_half) || LMud_CStr_FindAndPartition(ptr, ":", &end_first_half, &second_half)) {
                LMud_StringBuilder_Create(&builder2);
                {
                    LMud_StringBuilder_AppendSlice(&builder2, ptr, end_first_half);
                    package = LMud_Lisp_PackageByNameUpcase(lisp, LMud_StringBuilder_GetStatic(&builder2));
                    *result = LMud_Lisp_InternUpcaseInPackage(lisp, package, second_half);
                }
                LMud_StringBuilder_Destroy(&builder2);
            } else {
                *result = LMud_Lisp_InternUpcase(lisp, ptr);
            }
        }
    }
    LMud_StringBuilder_Destroy(&builder);

    return true;
}

bool LMud_Lisp_ReadKeyword(struct LMud_Lisp* lisp, struct LMud_InputStream* stream, LMud_Any* result)
{
    struct LMud_StringBuilder  builder;
    LMud_Any                   value;

    LMud_StringBuilder_Create(&builder);
    {
        LMud_Lisp_ReadUntilBreakingChar(lisp, stream, &builder);
        value = LMud_Lisp_InternKeywordUpcase(lisp, LMud_StringBuilder_GetStatic(&builder));
    }
    LMud_StringBuilder_Destroy(&builder);

    *result = value;

    return true;
}

bool LMud_Lisp_ReadCharacter(struct LMud_Lisp* lisp, struct LMud_InputStream* stream, LMud_Any* result)
{
    struct LMud_StringBuilder  builder;
    LMud_Rune                  rune;
    char*                      ptr;
    bool                       success;

    success = false;

    LMud_StringBuilder_Create(&builder);
    {
        LMud_Lisp_ReadUntilBreakingChar(lisp, stream, &builder);
        ptr = (char*) LMud_StringBuilder_GetStatic(&builder);

        success = LMud_Rune_ByName(ptr, &rune);
    }
    LMud_StringBuilder_Destroy(&builder);

    *result = LMud_Any_FromCharacter(rune);

    return success;
}

bool LMud_Lisp_Read(struct LMud_Lisp* lisp, struct LMud_InputStream* stream, LMud_Any* result)
{
    LMud_InputStream_SkipIf(stream, &LMud_Lisp_Read_IsWhitespace);

    if (LMud_InputStream_Eof(stream)) {
        *result = LMud_Lisp_Nil(lisp);
        return false;
    } else if (LMud_InputStream_CheckStr(stream, ";")) {
        LMud_InputStream_SkipIf(stream, &LMud_Lisp_Read_IsNotNewline);
        return LMud_Lisp_Read(lisp, stream, result);
    } else if (LMud_InputStream_CheckStr(stream, "#'")) {
        if (LMud_Lisp_Read(lisp, stream, result)) {
            *result = LMud_Lisp_QuoteFunction(lisp, *result);
            return true;
        }
    } else if (LMud_InputStream_CheckStr(stream, "'")) {
        if (LMud_Lisp_Read(lisp, stream, result)) {
            *result = LMud_Lisp_Quote(lisp, *result);
            return true;
        }
    } else if (LMud_InputStream_CheckStr(stream, "#\\")) {
        return LMud_Lisp_ReadCharacter(lisp, stream, result);
    } else if (LMud_InputStream_CheckStr(stream, "\"")) {
        return LMud_Lisp_ReadString(lisp, stream, result);
    } else if (LMud_InputStream_CheckStr(stream, "(")) {
        return LMud_Lisp_ReadList(lisp, stream, result);
    } else if (LMud_InputStream_CheckStr(stream, ":")) {
        return LMud_Lisp_ReadKeyword(lisp, stream, result);
    } else {
        return LMud_Lisp_ReadAtom(lisp, stream, result);
    }

    return false;
}
