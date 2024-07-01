
#pragma once

#include <lmud/defs.h>
#include <lmud/lmud.h>

#include <lmud/lisp/any.h>
#include <lmud/net/connection.h>

void LMud_NotifyIncomingConnection(struct LMud* self, LMud_Any startup_function, struct LMud_Connection* connection);

void LMud_Mark(struct LMud_GC* gc, struct LMud* self);
