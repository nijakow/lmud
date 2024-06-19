
#include <lmud/lisp/lisp.h>
#include <lmud/util/stringbuilder.h>

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

void LMud_Lisp_Print(struct LMud_Lisp* lisp, LMud_Any object, FILE* stream, bool escaped)
{
    void*  pointer;

    if (LMud_Any_IsInteger(object)) {
        fprintf(stream, "%d", LMud_Any_AsInteger(object));
    } else if (LMud_Any_IsPointer(object)) {
        pointer = LMud_Any_AsPointer(object);

        if (LMud_Lisp_IsConsPointer(lisp, pointer)) {
            LMud_Lisp_PrintList(lisp, object, stream, escaped);
        } else if (LMud_Lisp_IsStringPointer(lisp, pointer)) {
            if (escaped) {
                fprintf(stream, "\"%s\"", ((struct LMud_String*) pointer)->chars);
            } else {
                fprintf(stream, "%s", ((struct LMud_String*) pointer)->chars);
            }
        } else if (LMud_Lisp_IsSymbolPointer(lisp, pointer)) {
            LMud_Lisp_Print(lisp, ((struct LMud_Symbol*) pointer)->name, stream, false);
        }
    } else {
        fprintf(stream, "#<?>");
    }
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


LMud_Any LMud_Lisp_ReadList(struct LMud_Lisp* lisp, struct LMud_InputStream* stream)
{
    LMud_Any  value;

    LMud_InputStream_SkipIf(stream, &LMud_Lisp_Read_IsWhitespace);

    if (LMud_InputStream_CheckStr(stream, ". ")) {
         value = LMud_Lisp_Read(lisp, stream);
         if (!LMud_InputStream_CheckStr(stream, ")")) {
            // TODO: Read error
         }
    } else if (LMud_InputStream_CheckStr(stream, ")")) {
        value = LMud_Lisp_Nil(lisp);
    } else {
        value = LMud_Lisp_Read(lisp, stream);
        value = LMud_Lisp_Cons(
            lisp,
            value,
            LMud_Lisp_ReadList(lisp, stream)
        );
    }

    return value;
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

LMud_Any LMud_Lisp_ReadString(struct LMud_Lisp* lisp, struct LMud_InputStream* stream)
{
    struct LMud_StringBuilder  builder;
    LMud_Any                   value;

    LMud_StringBuilder_Create(&builder);
    LMud_Lisp_ReadEscaped(lisp, stream, &builder, "\"");
    value = LMud_Lisp_String(lisp, LMud_StringBuilder_GetStatic(&builder));
    LMud_StringBuilder_Destroy(&builder);

    return value;
}

LMud_Any LMud_Lisp_ReadAtom(struct LMud_Lisp* lisp, struct LMud_InputStream* stream)
{
    char      buffer[LMud_SYMBOL_NAME_LENGTH + 1];
    char*     ptr;
    char*     end;
    char      c;
    LMud_Any  value;

    ptr = &buffer[0];
    end = &buffer[sizeof(buffer) - 1];

    while (!LMud_InputStream_Eof(stream))
    {
        if (ptr >= end)
            break;

        c = LMud_InputStream_Get(stream);

        if (LMud_Lisp_Read_IsBreakingChar(c))
            break;

        LMud_InputStream_Advance(stream);

        *(ptr++) = c;
    }

    *(ptr++) = '\0';

    if (LMud_Lisp_ParseInt(lisp, buffer, &value)) {
        return value;
    } else {
        return LMud_Lisp_InternUpcase(lisp, buffer);
    }
}

LMud_Any LMud_Lisp_Read(struct LMud_Lisp* lisp, struct LMud_InputStream* stream)
{
    LMud_InputStream_SkipIf(stream, &LMud_Lisp_Read_IsWhitespace);

    if (LMud_InputStream_Eof(stream)) {
        // TODO: Read error
        return LMud_Lisp_Nil(lisp);
    } else if (LMud_InputStream_CheckStr(stream, "#'")) {
        return LMud_Lisp_QuoteFunction(lisp, LMud_Lisp_Read(lisp, stream));
    } else if (LMud_InputStream_CheckStr(stream, "'")) {
        return LMud_Lisp_Quote(lisp, LMud_Lisp_Read(lisp, stream));
    } else if (LMud_InputStream_CheckStr(stream, "\"")) {
        return LMud_Lisp_ReadString(lisp, stream);
    } else if (LMud_InputStream_CheckStr(stream, "(")) {
        return LMud_Lisp_ReadList(lisp, stream);
    } else {
        return LMud_Lisp_ReadAtom(lisp, stream);
    }
}
