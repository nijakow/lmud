
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/lisp/objects.h>
#include <lmud/lisp/runtime/scheduler.h>
#include <lmud/util/stream.h>


struct LMud_Constants
{
    LMud_Any  default_package;
    LMud_Any  keyword_package;

    LMud_Any  nil;
    LMud_Any  t;
    LMud_Any  quote;
    LMud_Any  function;

    LMud_Any  custom_dispatcher_function;
    LMud_Any  new_connection_function;
};

bool LMud_Constants_Create(struct LMud_Constants* self, struct LMud_Lisp* lisp);
void LMud_Constants_Destroy(struct LMud_Constants* self);
void LMud_Constants_Mark(struct LMud_GC* gc, struct LMud_Constants* self);


struct LMud_Lisp
{
    struct LMud*             mud;
    struct LMud_Objects      objects;
    struct LMud_Constants    constants;
    struct LMud_Scheduler    scheduler;
    struct LMud_InputStream  standard_input;
};

bool LMud_Lisp_Create(struct LMud_Lisp* self, struct LMud* mud);
void LMud_Lisp_Destroy(struct LMud_Lisp* self);
void LMud_Lisp_Mark(struct LMud_GC* gc, struct LMud_Lisp* self);

struct LMud_Objects*   LMud_Lisp_Objects(struct LMud_Lisp* self);
struct LMud_Scheduler* LMud_Lisp_Scheduler(struct LMud_Lisp* self);
struct LMud_Types*     LMud_Lisp_Types(struct LMud_Lisp* self);

bool LMud_Lisp_IsArrayPointer(struct LMud_Lisp* self, void* object);
bool LMud_Lisp_IsBuiltinPointer(struct LMud_Lisp* self, void* object);
bool LMud_Lisp_IsBytesPointer(struct LMud_Lisp* self, void* object);
bool LMud_Lisp_IsClosurePointer(struct LMud_Lisp* self, void* object);
bool LMud_Lisp_IsConsPointer(struct LMud_Lisp* self, void* object);
bool LMud_Lisp_IsCustomPointer(struct LMud_Lisp* self, void* object);
bool LMud_Lisp_IsFunctionPointer(struct LMud_Lisp* self, void* object);
bool LMud_Lisp_IsPackagePointer(struct LMud_Lisp* self, void* object);
bool LMud_Lisp_IsPortPointer(struct LMud_Lisp* self, void* object);
bool LMud_Lisp_IsProcessPointer(struct LMud_Lisp* self, void* object);
bool LMud_Lisp_IsRatioPointer(struct LMud_Lisp* self, void* object);
bool LMud_Lisp_IsStringPointer(struct LMud_Lisp* self, void* object);
bool LMud_Lisp_IsSymbolPointer(struct LMud_Lisp* self, void* object);

bool LMud_Lisp_IsArray(struct LMud_Lisp* self, LMud_Any value);
bool LMud_Lisp_IsBuiltin(struct LMud_Lisp* self, LMud_Any value);
bool LMud_Lisp_IsBytes(struct LMud_Lisp* self, LMud_Any value);
bool LMud_Lisp_IsClosure(struct LMud_Lisp* self, LMud_Any value);
bool LMud_Lisp_IsCons(struct LMud_Lisp* self, LMud_Any value);
bool LMud_Lisp_IsCustom(struct LMud_Lisp* self, LMud_Any value);
bool LMud_Lisp_IsFunction(struct LMud_Lisp* self, LMud_Any value);
bool LMud_Lisp_IsPackage(struct LMud_Lisp* self, LMud_Any value);
bool LMud_Lisp_IsPort(struct LMud_Lisp* self, LMud_Any value);
bool LMud_Lisp_IsProcess(struct LMud_Lisp* self, LMud_Any value);
bool LMud_Lisp_IsRatio(struct LMud_Lisp* self, LMud_Any value);
bool LMud_Lisp_IsString(struct LMud_Lisp* self, LMud_Any value);
bool LMud_Lisp_IsSymbol(struct LMud_Lisp* self, LMud_Any value);
bool LMud_Lisp_IsGensym(struct LMud_Lisp* self, LMud_Any value);

bool LMud_Lisp_IsNil(struct LMud_Lisp* self, LMud_Any value);

LMud_Any LMud_Lisp_T(struct LMud_Lisp* self);
LMud_Any LMud_Lisp_Nil(struct LMud_Lisp* self);
LMud_Any LMud_Lisp_Boolean(struct LMud_Lisp* self, bool value);
bool     LMud_Lisp_Truthy(struct LMud_Lisp* self, LMud_Any value);

LMud_Any LMud_Lisp_CustomDispatcherFunction(struct LMud_Lisp* self);
void     LMud_Lisp_SetCustomDispatcherFunction(struct LMud_Lisp* self, LMud_Any value);
void     LMud_Lisp_SetNewConnectionFunction(struct LMud_Lisp* self, LMud_Any value);

LMud_Any LMud_Lisp_MakeArray(struct LMud_Lisp* self, LMud_Size size, LMud_Any fill);
LMud_Any LMud_Lisp_MakeArray_FromData(struct LMud_Lisp* self, LMud_Size size, LMud_Any* data);
LMud_Any LMud_Lisp_Builtin(struct LMud_Lisp* self, const char* name, LMud_BuiltinFunction function);
LMud_Any LMud_Lisp_MakeBytes(struct LMud_Lisp* self, LMud_Size size);
LMud_Any LMud_Lisp_MakeBytes_FromData(struct LMud_Lisp* self, LMud_Size size, const char* data);
LMud_Any LMud_Lisp_Closure(struct LMud_Lisp* self, struct LMud_Function* function, struct LMud_Frame* lexical);
LMud_Any LMud_Lisp_Cons(struct LMud_Lisp* self, LMud_Any car, LMud_Any cdr);
LMud_Any LMud_Lisp_Custom(struct LMud_Lisp* self, LMud_Any meta, LMud_Any* slots, LMud_Size size);
LMud_Any LMud_Lisp_Function(struct LMud_Lisp* self, struct LMud_ArgInfo info, LMud_Any bytecodes, LMud_Any constants, LMud_Any source_code);
LMud_Any LMud_Lisp_Package(struct LMud_Lisp* self, LMud_Any name);
LMud_Any LMud_Lisp_PackageByName(struct LMud_Lisp* self, const char* name);
LMud_Any LMud_Lisp_PackageByNameUpcase(struct LMud_Lisp* self, const char* name);
LMud_Any LMud_Lisp_Port(struct LMud_Lisp* self, struct LMud_Connection* connection);
LMud_Any LMud_Lisp_Process(struct LMud_Lisp* self, struct LMud_Fiber* fiber);
LMud_Any LMud_Lisp_Ratio(struct LMud_Lisp* self, LMud_Any numerator, LMud_Any denominator);
LMud_Any LMud_Lisp_String(struct LMud_Lisp* self, const char* text);
LMud_Any LMud_Lisp_InternInPackage(struct LMud_Lisp* self, LMud_Any package, const char* name);
LMud_Any LMud_Lisp_InternUpcaseInPackage(struct LMud_Lisp* self, LMud_Any package, const char* name);
LMud_Any LMud_Lisp_Intern(struct LMud_Lisp* self, const char* name);
LMud_Any LMud_Lisp_InternUpcase(struct LMud_Lisp* self, const char* name);
LMud_Any LMud_Lisp_InternKeyword(struct LMud_Lisp* self, const char* name);
LMud_Any LMud_Lisp_InternKeywordUpcase(struct LMud_Lisp* self, const char* name);
LMud_Any LMud_Lisp_EasyIntern(struct LMud_Lisp* self, const char* package_name, const char* name);
LMud_Any LMud_Lisp_EasyUpcaseIntern(struct LMud_Lisp* self, const char* package_name, const char* name);
LMud_Any LMud_Lisp_ReinternAsKeyword(struct LMud_Lisp* self, LMud_Any symbol);
LMud_Any LMud_Lisp_InternInPackageLL(struct LMud_Lisp* self, LMud_Any package, LMud_Any name);
LMud_Any LMud_Lisp_Gensym(struct LMud_Lisp* self);

LMud_Any LMud_Lisp_Car(struct LMud_Lisp* self, LMud_Any value);
LMud_Any LMud_Lisp_Cdr(struct LMud_Lisp* self, LMud_Any value);
LMud_Any LMud_Lisp_Caar(struct LMud_Lisp* self, LMud_Any value);
LMud_Any LMud_Lisp_Cadr(struct LMud_Lisp* self, LMud_Any value);
LMud_Any LMud_Lisp_Cdar(struct LMud_Lisp* self, LMud_Any value);
LMud_Any LMud_Lisp_Cddr(struct LMud_Lisp* self, LMud_Any value);

bool LMud_Lisp_Rplaca(struct LMud_Lisp* self, LMud_Any cons, LMud_Any value);
bool LMud_Lisp_Rplacd(struct LMud_Lisp* self, LMud_Any cons, LMud_Any value);

bool     LMud_Lisp_TakeNext(struct LMud_Lisp* self, LMud_Any* value, LMud_Any* result);
bool     LMud_Lisp_Nth(struct LMud_Lisp* self, LMud_Any value, LMud_Size index, LMud_Any* result);

bool LMud_Lisp_Aref(struct LMud_Lisp* self, LMud_Any object, LMud_Any index, LMud_Any* result);
bool LMud_Lisp_Aset(struct LMud_Lisp* self, LMud_Any object, LMud_Any index, LMud_Any value);

LMud_Any LMud_Lisp_Quote(struct LMud_Lisp* self, LMud_Any value);
LMud_Any LMud_Lisp_QuoteFunction(struct LMud_Lisp* self, LMud_Any value);

bool LMud_Lisp_Compile(struct LMud_Lisp* self, LMud_Any expression, LMud_Any* result);

void LMud_Lisp_InstallBuiltin(struct LMud_Lisp* self, const char* name, LMud_BuiltinFunction function);
void LMud_Lisp_InstallPackagedBuiltin(struct LMud_Lisp* self, const char* package_name, const char* name, LMud_BuiltinFunction function);

void LMud_Lisp_GarbageCollect(struct LMud_Lisp* self, struct LMud_GCStats* stats);

bool LMud_Lisp_NeedsControlBackImmediately(struct LMud_Lisp* self);
void LMud_Lisp_PeriodicInterrupt(struct LMud_Lisp* self);
void LMud_Lisp_Tick(struct LMud_Lisp* self);

bool     LMud_Lisp_Kickstart(struct LMud_Lisp* self, LMud_Any function);
bool     LMud_Lisp_KickstartNewConnectionTask(struct LMud_Lisp* self, LMud_Any function, struct LMud_Connection* connection);
LMud_Any LMud_Lisp_KickstartProcess(struct LMud_Lisp* self, LMud_Any function, LMud_Any* arguments, LMud_Size argument_count);

bool LMud_Lisp_LoadFile(struct LMud_Lisp* self, const char* filename, LMud_Any* result);
