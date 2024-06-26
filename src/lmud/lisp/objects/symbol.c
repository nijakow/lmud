
#include <lmud/lisp/lisp.h>
#include <lmud/lisp/objects.h>
#include <lmud/lisp/objects/string.h>

#include "symbol.h"

void LMud_SymbolTable_Create(struct LMud_SymbolTable* self)
{
    self->symbols = NULL;
}

void LMud_SymbolTable_Destroy(struct LMud_SymbolTable* self)
{
    while (self->symbols != NULL)
    {
        LMud_Symbol_Unlink(self->symbols);
    }
}

struct LMud_Symbol* LMud_SymbolTable_Intern(struct LMud_SymbolTable* self, struct LMud_Objects* objects, const char* name, LMud_Any package)
{
    struct LMud_Symbol*  symbol;

    for (symbol = self->symbols; symbol != NULL; symbol = symbol->next)
    {
        if (LMud_String_Equals(LMud_Any_AsPointer(symbol->name), name))
        {
            return symbol;
        }
    }

    symbol = LMud_Objects_Allocate(objects, &objects->types.symbol, 0);

    if (symbol != NULL)
    {
        LMud_Symbol_Create(
            symbol,
            self,
            package,
            LMud_Any_FromPointer(LMud_Objects_String(objects, name)),
            LMud_Lisp_Nil(LMud_Objects_GetLisp(objects)),
            LMud_Lisp_Nil(LMud_Objects_GetLisp(objects)),
            LMud_Lisp_Nil(LMud_Objects_GetLisp(objects)),
            LMud_Lisp_Nil(LMud_Objects_GetLisp(objects))
        );
    }

    return symbol;
}

void LMud_SymbolTable_Dump(struct LMud_SymbolTable* self)
{
    struct LMud_Symbol*  symbol;

    printf("Symbol table:\n");

    for (symbol = self->symbols; symbol != NULL; symbol = symbol->next)
    {
        printf(" - %s\n", LMud_Symbol_Name(symbol));
    }
}


void LMud_Symbol_Create(struct LMud_Symbol* self, struct LMud_SymbolTable* table, LMud_Any package, LMud_Any name, LMud_Any value, LMud_Any function, LMud_Any macro, LMud_Any plist)
{
    self->prev = NULL;
    self->next = NULL;

    self->package = package;
    self->name    = name;

    self->value    = value;
    self->function = function;
    self->macro    = macro;

    self->plist    = plist;

    self->gensym   = false;

    if (table != NULL)
    {
        LMud_Symbol_Link(self, table);
    }
}

void LMud_Symbol_Destroy(struct LMud_Symbol* self)
{
    LMud_Symbol_Unlink(self);
}


void LMud_Symbol_Unlink(struct LMud_Symbol* self)
{
    if (self->prev != NULL)
        *self->prev = self->next;
    if (self->next != NULL)
        self->next->prev = self->prev;

    self->prev = NULL;
    self->next = NULL;
}

void LMud_Symbol_LinkIntoList(struct LMud_Symbol* self, struct LMud_Symbol** list)
{
    LMud_Symbol_Unlink(self);

    self->prev =  list;
    self->next = *list;

    if (*list != NULL)
    {
        (*list)->prev = &self->next;
    }

    *list = self;
}

void LMud_Symbol_Link(struct LMud_Symbol* self, struct LMud_SymbolTable* table)
{
    LMud_Symbol_LinkIntoList(self, &table->symbols);
}

bool LMud_Symbol_IsGensym(struct LMud_Symbol* self)
{
    return self->gensym;
}

void LMud_Symbol_MakeGensym(struct LMud_Symbol* self)
{
    self->gensym = true;
}

LMud_Any LMud_Symbol_Package(struct LMud_Symbol* self)
{
    return self->package;
}

const char* LMud_Symbol_Name(struct LMud_Symbol* self)
{
    return LMud_String_Chars((struct LMud_String*) LMud_Any_AsPointer(self->name));
}

LMud_Any LMud_Symbol_Value(struct LMud_Symbol* self)
{
    return self->value;
}

LMud_Any LMud_Symbol_Function(struct LMud_Symbol* self)
{
    return self->function;
}

LMud_Any LMud_Symbol_Macro(struct LMud_Symbol* self)
{
    return self->macro;
}

LMud_Any LMud_Symbol_Plist(struct LMud_Symbol* self)
{
    return self->plist;
}


void LMud_Symbol_SetValue(struct LMud_Symbol* self, LMud_Any value)
{
    self->value = value;
}

void LMud_Symbol_SetFunction(struct LMud_Symbol* self, LMud_Any function)
{
    self->function = function;
}

void LMud_Symbol_SetMacro(struct LMud_Symbol* self, LMud_Any macro)
{
    self->macro = macro;
}

void LMud_Symbol_SetPlist(struct LMud_Symbol* self, LMud_Any plist)
{
    self->plist = plist;
}


void LMud_Symbol_MakeConstant(struct LMud_Symbol* self)
{
    LMud_Symbol_SetValue(self, LMud_Any_FromPointer(self));
}
