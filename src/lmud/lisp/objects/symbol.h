
#pragma once

#include <lmud/lisp/base.h>

struct LMud_Symbol;

struct LMud_SymbolTable
{
    struct LMud_Symbol*  symbols;
};

void LMud_SymbolTable_Create(struct LMud_SymbolTable* self);
void LMud_SymbolTable_Destroy(struct LMud_SymbolTable* self);

struct LMud_Symbol* LMud_SymbolTable_Intern(struct LMud_SymbolTable* self, struct LMud_Objects* objects, const char* name);


struct LMud_Symbol
{
    struct LMud_Object   _;

    struct LMud_Symbol** prev;
    struct LMud_Symbol*  next;

    LMud_Any             name;

    LMud_Any             value;
    LMud_Any             function;
};

void LMud_Symbol_Create(struct LMud_Symbol* self, struct LMud_SymbolTable* table, LMud_Any name);
void LMud_Symbol_Destroy(struct LMud_Symbol* self);

void LMud_Symbol_Unlink(struct LMud_Symbol* self);
void LMud_Symbol_LinkIntoList(struct LMud_Symbol* self, struct LMud_Symbol** list);
void LMud_Symbol_Link(struct LMud_Symbol* self, struct LMud_SymbolTable* table);
