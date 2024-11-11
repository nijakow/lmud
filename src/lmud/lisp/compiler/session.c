
#include <lmud/lisp/lisp.h>

#include "session.h"


void LMud_CompilerCache_Create(struct LMud_CompilerCache* self, struct LMud_CompilerSession* session)
{
    struct LMud_Lisp*  lisp;

    lisp = LMud_CompilerSession_GetLisp(session);

    self->symbol_declare        = LMud_Lisp_Intern(lisp, "DECLARE");
    self->symbol_quote          = LMud_Lisp_Intern(lisp, "QUOTE");
    self->symbol_function       = LMud_Lisp_Intern(lisp, "FUNCTION");
    self->symbol_lambda         = LMud_Lisp_Intern(lisp, "LAMBDA");
    self->symbol_block          = LMud_Lisp_Intern(lisp, "BLOCK");
    self->symbol_progn          = LMud_Lisp_Intern(lisp, "PROGN");
    self->symbol_setq           = LMud_Lisp_Intern(lisp, "SETQ");
    self->symbol_let            = LMud_Lisp_Intern(lisp, "LET");
    self->symbol_flet           = LMud_Lisp_Intern(lisp, "FLET");
    self->symbol_labels         = LMud_Lisp_Intern(lisp, "LABELS");
    self->symbol_if             = LMud_Lisp_Intern(lisp, "IF");
    self->symbol_while          = LMud_Lisp_Intern(lisp, "WHILE");
    self->symbol_mvb            = LMud_Lisp_Intern(lisp, "MULTIPLE-VALUE-BIND");
    self->symbol_mvl            = LMud_Lisp_Intern(lisp, "MULTIPLE-VALUE-LIST");
    self->symbol_return_from    = LMud_Lisp_Intern(lisp, "RETURN-FROM");
    self->symbol_unwind_protect = LMud_Lisp_Intern(lisp, "UNWIND-PROTECT");
    self->symbol_signal_handler = LMud_Lisp_Intern(lisp, "%SIGNAL-HANDLER");

    self->symbol_andrest       = LMud_Lisp_Intern(lisp, "&REST");
    self->symbol_andbody       = LMud_Lisp_Intern(lisp, "&BODY");
    self->symbol_andoptional   = LMud_Lisp_Intern(lisp, "&OPTIONAL");
    self->symbol_andkey        = LMud_Lisp_Intern(lisp, "&KEY");
    self->symbol_andignorerest = LMud_Lisp_Intern(lisp, "&IGNORE-REST");

    self->symbol_function_name = LMud_Lisp_Intern(lisp, "FUNCTION-NAME");
    self->symbol_macro_name    = LMud_Lisp_Intern(lisp, "MACRO-NAME");
    self->symbol_method_name   = LMud_Lisp_Intern(lisp, "METHOD-NAME");
}

void LMud_CompilerCache_Destroy(struct LMud_CompilerCache* self)
{
    (void) self;
}



void LMud_CompilerSession_Create(struct LMud_CompilerSession* self, struct LMud_Lisp* lisp)
{
    self->lisp = lisp;

    LMud_CompilerCache_Create(&self->cache, self);
}

void LMud_CompilerSession_Destroy(struct LMud_CompilerSession* self)
{
    (void) self;
}


struct LMud_Lisp* LMud_CompilerSession_GetLisp(struct LMud_CompilerSession* self)
{
    return self->lisp;
}

struct LMud_CompilerCache* LMud_CompilerSession_GetCache(struct LMud_CompilerSession* self)
{
    return &self->cache;
}
