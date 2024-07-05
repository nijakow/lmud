
#pragma once

#include <lmud/lisp/base.h>

struct LMud_Symbol;

struct LMud_SymbolTable
{
    struct LMud_Symbol*  symbols;
};

void LMud_SymbolTable_Create(struct LMud_SymbolTable* self);
void LMud_SymbolTable_Destroy(struct LMud_SymbolTable* self);
void LMud_SymbolTable_Mark(struct LMud_GC* gc, struct LMud_SymbolTable* self);

struct LMud_Symbol* LMud_SymbolTable_Intern(struct LMud_SymbolTable* self, struct LMud_Objects* objects, const char* name, LMud_Any package);

void LMud_SymbolTable_Dump(struct LMud_SymbolTable* self);


enum LMud_SymbolSlot
{
    LMud_SymbolSlot_Value,
    LMud_SymbolSlot_Function,
    LMud_SymbolSlot_Macro,
    LMud_SymbolSlot_Plist,
    LMud_SymbolSlot_COUNT
};

struct LMud_Symbol
{
    struct LMud_Symbol** prev;
    struct LMud_Symbol*  next;

    LMud_Any             package;
    LMud_Any             name;

    LMud_Any             slots[LMud_SymbolSlot_COUNT];

    bool                 gensym;
    bool                 locked;
};

void LMud_Symbol_Create(struct LMud_Symbol* self, struct LMud_SymbolTable* table, LMud_Any package, LMud_Any name, LMud_Any value, LMud_Any function, LMud_Any macro, LMud_Any plist);
void LMud_Symbol_Destroy(struct LMud_Symbol* self);
void LMud_Symbol_Mark(struct LMud_GC* gc, struct LMud_Symbol* self);
LMud_Size LMud_Symbol_CalculateSizeInBytes(struct LMud_Symbol* self);

void LMud_Symbol_Unlink(struct LMud_Symbol* self);
void LMud_Symbol_LinkIntoList(struct LMud_Symbol* self, struct LMud_Symbol** list);
void LMud_Symbol_Link(struct LMud_Symbol* self, struct LMud_SymbolTable* table);

bool LMud_Symbol_IsGensym(struct LMud_Symbol* self);
void LMud_Symbol_MakeGensym(struct LMud_Symbol* self);

bool LMud_Symbol_IsLocked(struct LMud_Symbol* self);
void LMud_Symbol_SetLocked(struct LMud_Symbol* self, bool value);

LMud_Any    LMud_Symbol_Package(struct LMud_Symbol* self);
LMud_Any    LMud_Symbol_Name(struct LMud_Symbol* self);
const char* LMud_Symbol_NameChars(struct LMud_Symbol* self);

bool    LMud_Symbol_GetSlot(struct LMud_Symbol* self, enum LMud_SymbolSlot slot, LMud_Any* value, bool override);
bool    LMud_Symbol_SetSlot(struct LMud_Symbol* self, enum LMud_SymbolSlot slot, LMud_Any value, bool override);

LMud_Any    LMud_Symbol_Value(struct LMud_Symbol* self);
LMud_Any    LMud_Symbol_Function(struct LMud_Symbol* self);
LMud_Any    LMud_Symbol_Macro(struct LMud_Symbol* self);
LMud_Any    LMud_Symbol_Plist(struct LMud_Symbol* self);

bool LMud_Symbol_SetValue(struct LMud_Symbol* self, LMud_Any value, bool override);
bool LMud_Symbol_SetFunction(struct LMud_Symbol* self, LMud_Any function, bool override);
bool LMud_Symbol_SetMacro(struct LMud_Symbol* self, LMud_Any macro, bool override);
bool LMud_Symbol_SetPlist(struct LMud_Symbol* self, LMud_Any plist, bool override);

void LMud_Symbol_MakeConstant(struct LMud_Symbol* self, bool override);

bool LMud_Symbol_IsWorthless(struct LMud_Symbol* self, struct LMud_Lisp* lisp);
