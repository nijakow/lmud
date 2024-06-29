
#pragma once

#include <lmud/defs.h>
#include <lmud/lmud.h>

#include <lmud/net/connection.h>

void LMud_NotifyIncomingConnection(struct LMud* self, struct LMud_Connection* connection);
