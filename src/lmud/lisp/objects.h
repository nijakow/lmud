
#pragma once

#include <lmud/lisp/object.h>

#include <lmud/lisp/objects/array.h>
#include <lmud/lisp/objects/bytes.h>
#include <lmud/lisp/objects/cons.h>
#include <lmud/lisp/objects/string.h>
#include <lmud/lisp/objects/symbol.h>


struct LMud_Types
{
    struct LMud_Type     array;
    struct LMud_Type     bytes;
    struct LMud_Type     cons;
    struct LMud_Type     string;
    struct LMud_Type     symbol;
};

void LMud_Types_Create(struct LMud_Types* self);
void LMud_Types_Destroy(struct LMud_Types* self);

bool LMud_Types_IsArray(struct LMud_Types* self, void* object);
bool LMud_Types_IsBytes(struct LMud_Types* self, void* object);
bool LMud_Types_IsCons(struct LMud_Types* self, void* object);
bool LMud_Types_IsString(struct LMud_Types* self, void* object);
bool LMud_Types_IsSymbol(struct LMud_Types* self, void* object);


struct LMud_Objects
{
    struct LMud_Object*      objects;
    struct LMud_SymbolTable  symbols;
    struct LMud_Types        types;
};

bool LMud_Objects_Create(struct LMud_Objects* self);
void LMud_Objects_Destroy(struct LMud_Objects* self);

void* LMud_Objects_Allocate(struct LMud_Objects* self, struct LMud_Type* type, LMud_Size extra);

struct LMud_Array*  LMud_Objects_MakeArray(struct LMud_Objects* self, LMud_Size size, LMud_Any fill);

struct LMud_Bytes*  LMud_Objects_MakeBytes(struct LMud_Objects* self, LMud_Size size);
struct LMud_Bytes*  LMud_Objects_MakeBytes_FromData(struct LMud_Objects* self, LMud_Size size, const char* data);

struct LMud_Cons*   LMud_Objects_Cons(struct LMud_Objects* self, LMud_Any car, LMud_Any cdr);

struct LMud_String* LMud_Objects_String(struct LMud_Objects* self, const char* text);

struct LMud_Symbol* LMud_Objects_PrimitiveIntern(struct LMud_Objects* self, const char* name);
struct LMud_Symbol* LMud_Objects_Intern(struct LMud_Objects* self, const char* name);
