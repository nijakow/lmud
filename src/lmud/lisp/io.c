
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


LMud_Any LMud_Lisp_Read(struct LMud_Lisp* lisp, struct LMud_InputStream* stream)
{
    (void) stream;
    return LMud_Lisp_Nil(lisp);
}
