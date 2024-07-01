
#pragma once

#include <lmud/defs.h>
#include <lmud/lisp/any.h>
#include <lmud/net/connection.h>
#include <lmud/net/servers.h>

struct LMud_Net
{
    struct LMud*             mud;
    struct LMud_Servers      servers;
    struct LMud_Connections  connections;
};

bool LMud_Net_Create(struct LMud_Net* self, struct LMud* mud);
void LMud_Net_Destroy(struct LMud_Net* self);

void LMud_Net_Mark(struct LMud_GC* gc, struct LMud_Net* self);

bool LMud_Net_OpenV4(struct LMud_Net* self, const char* address, LMud_Port port, LMud_Any startup_function);
bool LMud_Net_OpenV6(struct LMud_Net* self, const char* address, LMud_Port port, LMud_Any startup_function);

bool LMud_Net_RegisterClientFileDescriptor(struct LMud_Net* self, int fd, bool close_on_error, struct LMud_Connection** connection);
bool LMud_Net_RegisterClientSocket(struct LMud_Net* self, LMud_Socket socket, bool close_on_error, struct LMud_Connection** connection);

bool LMud_Net_IncomingConnection(struct LMud_Net* self, struct LMud_Server* from, LMud_Socket socket);

void LMud_Net_Tick(struct LMud_Net* self, bool block);
