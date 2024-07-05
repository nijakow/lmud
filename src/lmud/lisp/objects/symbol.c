
#include <lmud/lisp/lisp.h>
#include <lmud/lisp/objects.h>
#include <lmud/lisp/objects/string.h>
#include <lmud/lisp/gc.h>

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

void LMud_SymbolTable_Mark(struct LMud_GC* gc, struct LMud_SymbolTable* self)
{
    struct LMud_Symbol*  symbol;

    for (symbol = self->symbols; symbol != NULL; symbol = symbol->next)
    {
        if (LMud_Symbol_IsWorthless(symbol, gc->lisp)) {
            LMud_Debugf(gc->lisp->mud, LMud_LogLevel_FULL_DEBUG, "Symbol %s is considered worthless and might be collected.", LMud_Symbol_NameChars(symbol));
        } else {
            LMud_GC_MarkObject(gc, symbol);
        }
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
        printf(" - %s\n", LMud_Symbol_NameChars(symbol));
    }
}



static LMud_Any LMud_Symbol_GetSlot_FORCE(struct LMud_Symbol* self, enum LMud_SymbolSlot slot);
static void     LMud_Symbol_SetSlot_FORCE(struct LMud_Symbol* self, enum LMud_SymbolSlot slot, LMud_Any value);

void LMud_Symbol_Create(struct LMud_Symbol* self, struct LMud_SymbolTable* table, LMud_Any package, LMud_Any name, LMud_Any value, LMud_Any function, LMud_Any macro, LMud_Any plist)
{
    self->prev = NULL;
    self->next = NULL;

    self->package = package;
    self->name    = name;

    LMud_Symbol_SetSlot_FORCE(self, LMud_SymbolSlot_Value,    value);
    LMud_Symbol_SetSlot_FORCE(self, LMud_SymbolSlot_Function, function);
    LMud_Symbol_SetSlot_FORCE(self, LMud_SymbolSlot_Macro,    macro);
    LMud_Symbol_SetSlot_FORCE(self, LMud_SymbolSlot_Plist,    plist);

    self->gensym   = false;
    self->locked   = false;

    if (table != NULL)
    {
        LMud_Symbol_Link(self, table);
    }
}

void LMud_Symbol_Destroy(struct LMud_Symbol* self)
{
    LMud_Symbol_Unlink(self);
}

void LMud_Symbol_Mark(struct LMud_GC* gc, struct LMud_Symbol* self)
{
    LMud_Size  index;

    LMud_GC_MarkAny(gc, self->package);
    LMud_GC_MarkAny(gc, self->name);

    for (index = 0; index < LMud_SymbolSlot_COUNT; index++)
    {
        LMud_GC_MarkAny(gc, self->slots[index]);
    }
}

LMud_Size LMud_Symbol_CalculateSizeInBytes(struct LMud_Symbol* self)
{
    (void) self;
    return sizeof(struct LMud_Symbol);
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

bool LMud_Symbol_IsLocked(struct LMud_Symbol* self)
{
    return self->locked;
}

void LMud_Symbol_SetLocked(struct LMud_Symbol* self, bool value)
{
    self->locked = value;
}

LMud_Any LMud_Symbol_Package(struct LMud_Symbol* self)
{
    return self->package;
}

LMud_Any LMud_Symbol_Name(struct LMud_Symbol* self)
{
    return self->name;
}

const char* LMud_Symbol_NameChars(struct LMud_Symbol* self)
{
    return LMud_String_Chars((struct LMud_String*) LMud_Any_AsPointer(self->name));
}


static LMud_Any LMud_Symbol_GetSlot_FORCE(struct LMud_Symbol* self, enum LMud_SymbolSlot slot)
{
    return self->slots[slot];
}

static void LMud_Symbol_SetSlot_FORCE(struct LMud_Symbol* self, enum LMud_SymbolSlot slot, LMud_Any value)
{
    self->slots[slot] = value;
}

bool LMud_Symbol_GetSlot(struct LMud_Symbol* self, enum LMud_SymbolSlot slot, LMud_Any* value, bool override)
{
    (void) override;
    /*
     * TODO: Check if the symbol was locked for the specific slot and user combination.
     */
    if (slot < 0 || slot >= LMud_SymbolSlot_COUNT)
        return false;
    *value = self->slots[slot];
    return true;
}

bool LMud_Symbol_SetSlot(struct LMud_Symbol* self, enum LMud_SymbolSlot slot, LMud_Any value, bool override)
{
    (void) override;
    /*
     * TODO: Check if the symbol was locked for the specific slot and user combination.
     */
    if (slot < 0 || slot >= LMud_SymbolSlot_COUNT)
        return false;
    self->slots[slot] = value;
    return true;
}


LMud_Any LMud_Symbol_Value(struct LMud_Symbol* self)
{
    return LMud_Symbol_GetSlot_FORCE(self, LMud_SymbolSlot_Value);
}

LMud_Any LMud_Symbol_Function(struct LMud_Symbol* self)
{
    return LMud_Symbol_GetSlot_FORCE(self, LMud_SymbolSlot_Function);
}

LMud_Any LMud_Symbol_Macro(struct LMud_Symbol* self)
{
    return LMud_Symbol_GetSlot_FORCE(self, LMud_SymbolSlot_Macro);
}

LMud_Any LMud_Symbol_Plist(struct LMud_Symbol* self)
{
    return LMud_Symbol_GetSlot_FORCE(self, LMud_SymbolSlot_Plist);
}


bool LMud_Symbol_SetValue(struct LMud_Symbol* self, LMud_Any value, bool override)
{
    if (LMud_Symbol_IsLocked(self) && !override)
        return false;
    LMud_Symbol_SetSlot_FORCE(self, LMud_SymbolSlot_Value, value);
    return true;
}

bool LMud_Symbol_SetFunction(struct LMud_Symbol* self, LMud_Any function, bool override)
{
    if (LMud_Symbol_IsLocked(self) && !override)
        return false;
    LMud_Symbol_SetSlot_FORCE(self, LMud_SymbolSlot_Function, function);
    return true;
}

bool LMud_Symbol_SetMacro(struct LMud_Symbol* self, LMud_Any macro, bool override)
{
    if (LMud_Symbol_IsLocked(self) && !override)
        return false;
    LMud_Symbol_SetSlot_FORCE(self, LMud_SymbolSlot_Macro, macro);
    return true;
}

bool LMud_Symbol_SetPlist(struct LMud_Symbol* self, LMud_Any plist, bool override)
{
    if (LMud_Symbol_IsLocked(self) && !override)
        return false;
    LMud_Symbol_SetSlot_FORCE(self, LMud_SymbolSlot_Plist, plist);
    return true;
}


void LMud_Symbol_MakeConstant(struct LMud_Symbol* self, bool override)
{
    LMud_Symbol_SetValue(self, LMud_Any_FromPointer(self), override);
}

bool LMud_Symbol_IsWorthless(struct LMud_Symbol* self, struct LMud_Lisp* lisp)
{
    return (LMud_Lisp_IsNil(lisp, LMud_Symbol_Value(self))
         && LMud_Lisp_IsNil(lisp, LMud_Symbol_Function(self))
         && LMud_Lisp_IsNil(lisp, LMud_Symbol_Macro(self))
         && LMud_Lisp_IsNil(lisp, LMud_Symbol_Plist(self)));
}
