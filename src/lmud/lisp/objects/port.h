
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/net/connection.h>

struct LMud_Port
{
    struct LMud_ConnectionRef  connection;
    
    char*                      pushbacks;
    LMud_Size                  pushbacks_alloc;
    LMud_Size                  pushbacks_fill;
};

void LMud_Port_Create(struct LMud_Port* self, struct LMud_Connection* connection);
void LMud_Port_Destroy(struct LMud_Port* self);
void LMud_Port_Mark(struct LMud_GC* gc, struct LMud_Port* self);
LMud_Size LMud_Port_CalculateSizeInBytes(struct LMud_Port* self);

bool LMud_Port_PushbackByte(struct LMud_Port* self, char byte);
bool LMud_Port_PushbackRune(struct LMud_Port* self, LMud_Rune rune);

bool LMud_Port_WriteByte(struct LMud_Port* self, char byte);

bool LMud_Port_FiberReadByte(struct LMud_Port* self, struct LMud_Fiber* fiber);
