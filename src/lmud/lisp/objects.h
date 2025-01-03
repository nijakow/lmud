
#pragma once

#include <lmud/lisp/object.h>

#include <lmud/lisp/objects/array.h>
#include <lmud/lisp/objects/builtin.h>
#include <lmud/lisp/objects/bytes.h>
#include <lmud/lisp/objects/closure.h>
#include <lmud/lisp/objects/cons.h>
#include <lmud/lisp/objects/custom.h>
#include <lmud/lisp/objects/function.h>
#include <lmud/lisp/objects/package.h>
#include <lmud/lisp/objects/port.h>
#include <lmud/lisp/objects/process.h>
#include <lmud/lisp/objects/ratio.h>
#include <lmud/lisp/objects/stackframe.h>
#include <lmud/lisp/objects/string.h>
#include <lmud/lisp/objects/symbol.h>


struct LMud_Types
{
    struct LMud_Type     array;
    struct LMud_Type     builtin;
    struct LMud_Type     bytes;
    struct LMud_Type     closure;
    struct LMud_Type     cons;
    struct LMud_Type     custom;
    struct LMud_Type     function;
    struct LMud_Type     package;
    struct LMud_Type     port;
    struct LMud_Type     process;
    struct LMud_Type     ratio;
    struct LMud_Type     stack_frame;
    struct LMud_Type     string;
    struct LMud_Type     symbol;
};

void LMud_Types_Create(struct LMud_Types* self);
void LMud_Types_Destroy(struct LMud_Types* self);

bool LMud_Types_IsArray(struct LMud_Types* self, void* object);
bool LMud_Types_IsBuiltin(struct LMud_Types* self, void* object);
bool LMud_Types_IsBytes(struct LMud_Types* self, void* object);
bool LMud_Types_IsClosure(struct LMud_Types* self, void* object);
bool LMud_Types_IsCons(struct LMud_Types* self, void* object);
bool LMud_Types_IsCustom(struct LMud_Types* self, void* object);
bool LMud_Types_IsFunction(struct LMud_Types* self, void* object);
bool LMud_Types_IsPackage(struct LMud_Types* self, void* object);
bool LMud_Types_IsPort(struct LMud_Types* self, void* object);
bool LMud_Types_IsProcess(struct LMud_Types* self, void* object);
bool LMud_Types_IsRatio(struct LMud_Types* self, void* object);
bool LMud_Types_IsStackFrame(struct LMud_Types* self, void* object);
bool LMud_Types_IsString(struct LMud_Types* self, void* object);
bool LMud_Types_IsSymbol(struct LMud_Types* self, void* object);


struct LMud_Objects
{
    struct LMud_Lisp*        lisp;
    struct LMud_Header*      objects;
    struct LMud_Package*     packages;
    struct LMud_Types        types;

    LMud_Size                bytes_allocated;
    LMud_Size                bytes_allocated_since_gc;
    LMud_Size                objects_allocated;
};

bool LMud_Objects_Create(struct LMud_Objects* self, struct LMud_Lisp* lisp);
void LMud_Objects_Destroy(struct LMud_Objects* self);

struct LMud_Lisp* LMud_Objects_GetLisp(struct LMud_Objects* self);

LMud_Size LMud_Objects_GetGcAllocationCounter(struct LMud_Objects* self);
void      LMud_Objects_ClearGcAllocationCounter(struct LMud_Objects* self);

void* LMud_Objects_Allocate(struct LMud_Objects* self, struct LMud_Type* type, LMud_Size extra);

struct LMud_Array*      LMud_Objects_MakeArray(struct LMud_Objects* self, LMud_Size size, LMud_Any fill);
struct LMud_Array*      LMud_Objects_MakeArray_FromData(struct LMud_Objects* self, LMud_Size size, LMud_Any* data);
struct LMud_Builtin*    LMud_Objects_Builtin(struct LMud_Objects* self, const char* name, LMud_BuiltinFunction function);
struct LMud_Bytes*      LMud_Objects_MakeBytes(struct LMud_Objects* self, LMud_Size size);
struct LMud_Bytes*      LMud_Objects_MakeBytes_FromData(struct LMud_Objects* self, LMud_Size size, const char* data);
struct LMud_Closure*    LMud_Objects_Closure(struct LMud_Objects* self, struct LMud_Function* function, struct LMud_Frame* lexical);
struct LMud_Cons*       LMud_Objects_Cons(struct LMud_Objects* self, LMud_Any car, LMud_Any cdr);
struct LMud_Custom*     LMud_Objects_Custom(struct LMud_Objects* self, LMud_Any meta, LMud_Any* slots, LMud_Size size);
struct LMud_Function*   LMud_Objects_Function(struct LMud_Objects* self, struct LMud_ArgInfo info, LMud_Any bytecodes, LMud_Any constants, LMud_Any source_code);
struct LMud_Package*    LMud_Objects_Package(struct LMud_Objects* self, LMud_Any name);
struct LMud_Port*       LMud_Objects_Port(struct LMud_Objects* self, struct LMud_Connection* connection);
struct LMud_Process*    LMud_Objects_Process(struct LMud_Objects* self, struct LMud_Fiber* fiber, struct LMud_Process** slot);
struct LMud_Ratio*      LMud_Objects_Ratio(struct LMud_Objects* self, LMud_Any numerator, LMud_Any denominator);
struct LMud_String*     LMud_Objects_String(struct LMud_Objects* self, const char* text);
struct LMud_StackFrame* LMud_Objects_StackFrame(struct LMud_Objects* self, struct LMud_Frame* frame, struct LMud_StackFrame** slot);
struct LMud_Symbol*     LMud_Objects_PrimitiveIntern(struct LMud_Objects* self, struct LMud_Package* package, const char* name);
struct LMud_Symbol*     LMud_Objects_Intern(struct LMud_Objects* self, struct LMud_Package* package, const char* name);
struct LMud_Symbol*     LMud_Objects_Gensym(struct LMud_Objects* self);
