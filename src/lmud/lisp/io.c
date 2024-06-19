
#include <lmud/lisp/lisp.h>

#include "io.h"


void LMud_Lisp_Print(struct LMud_Lisp* lisp, LMud_Any object, FILE* stream, bool escaped)
{
    void*  pointer;

    if (LMud_Any_IsInteger(object)) {
        fprintf(stream, "%d", LMud_Any_AsInteger(object));
    } else if (LMud_Any_IsPointer(object)) {
        pointer = LMud_Any_AsPointer(object);

        if (LMud_Lisp_IsCons(lisp, pointer)) {
            fprintf(stream, "(");
            LMud_Lisp_Print(lisp, ((struct LMud_Cons*) pointer)->car, stream, escaped);
            fprintf(stream, " . ");
            LMud_Lisp_Print(lisp, ((struct LMud_Cons*) pointer)->cdr, stream, escaped);
            fprintf(stream, ")");
        } else if (LMud_Lisp_IsString(lisp, pointer)) {
            if (escaped) {
                fprintf(stream, "\"%s\"", ((struct LMud_String*) pointer)->chars);
            } else {
                fprintf(stream, "%s", ((struct LMud_String*) pointer)->chars);
            }
        } else if (LMud_Lisp_IsSymbol(lisp, pointer)) {
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
        || (c == ')');
}


LMud_Any LMud_Lisp_ReadList(struct LMud_Lisp* lisp, struct LMud_InputStream* stream)
{
    LMud_Any  value;

    LMud_InputStream_SkipIf(stream, &LMud_Lisp_Read_IsWhitespace);

    if (LMud_InputStream_CheckStr(stream, ". ")) {
         value = LMud_Lisp_ReadList(lisp, stream);
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


LMud_Any LMud_Lisp_ReadAtom(struct LMud_Lisp* lisp, struct LMud_InputStream* stream)
{
    char   buffer[1024];
    char*  ptr;
    char*  end;
    char   c;

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

    return LMud_Lisp_Intern(lisp, buffer);
}

LMud_Any LMud_Lisp_Read(struct LMud_Lisp* lisp, struct LMud_InputStream* stream)
{
    LMud_InputStream_SkipIf(stream, &LMud_Lisp_Read_IsWhitespace);

    if (LMud_InputStream_Eof(stream)) {
        // TODO: Read error
        return LMud_Lisp_Nil(lisp);    
    } else if (LMud_InputStream_CheckStr(stream, "(")) {
        return LMud_Lisp_ReadList(lisp, stream);
    } else {
        return LMud_Lisp_ReadAtom(lisp, stream);
    }
}
