
#pragma once

#include <lmud/lisp/base.h>


struct LMud_CompilerSession;


struct LMud_CompilerCache
{
    LMud_Any                    symbol_symbol_value;
    LMud_Any                    symbol_set_symbol_value;
    LMud_Any                    symbol_set_symbol_function;

    LMud_Any                    symbol_declare;
    LMud_Any                    symbol_quote;
    LMud_Any                    symbol_function;
    LMud_Any                    symbol_lambda;
    LMud_Any                    symbol_block;
    LMud_Any                    symbol_progn;
    LMud_Any                    symbol_setq;
    LMud_Any                    symbol_let;
    LMud_Any                    symbol_flet;
    LMud_Any                    symbol_labels;
    LMud_Any                    symbol_if;
    LMud_Any                    symbol_while;
    LMud_Any                    symbol_mvb;
    LMud_Any                    symbol_mvl;
    LMud_Any                    symbol_return_from;
    LMud_Any                    symbol_unwind_protect;
    LMud_Any                    symbol_signal_handler;

    LMud_Any                    symbol_andrest;
    LMud_Any                    symbol_andbody;
    LMud_Any                    symbol_andoptional;
    LMud_Any                    symbol_andkey;
    LMud_Any                    symbol_andignorerest;

    LMud_Any                    symbol_function_name; // For DECLARE forms
    LMud_Any                    symbol_macro_name;    // For DECLARE forms
    LMud_Any                    symbol_method_name;   // For DECLARE forms
};

void LMud_CompilerCache_Create(struct LMud_CompilerCache* self, struct LMud_CompilerSession* session);
void LMud_CompilerCache_Destroy(struct LMud_CompilerCache* self);


struct LMud_CompilerSession
{
    struct LMud_Lisp*          lisp;
    struct LMud_CompilerCache  cache;
};

void LMud_CompilerSession_Create(struct LMud_CompilerSession* self, struct LMud_Lisp* lisp);
void LMud_CompilerSession_Destroy(struct LMud_CompilerSession* self);

struct LMud_Lisp*          LMud_CompilerSession_GetLisp(struct LMud_CompilerSession* self);
struct LMud_CompilerCache* LMud_CompilerSession_GetCache(struct LMud_CompilerSession* self);
