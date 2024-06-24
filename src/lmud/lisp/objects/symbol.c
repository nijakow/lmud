
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

struct LMud_Symbol* LMud_SymbolTable_Intern(struct LMud_SymbolTable* self, struct LMud_Objects* objects, const char* name)
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
        LMud_Symbol_Create(symbol, self, LMud_Any_FromPointer(LMud_Objects_String(objects, name)));
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


void LMud_Symbol_Create(struct LMud_Symbol* self, struct LMud_SymbolTable* table, LMud_Any name)
{
    self->prev = NULL;
    self->next = NULL;

    self->name = name;

    // TODO: value, function

    LMud_Symbol_Link(self, table);
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
