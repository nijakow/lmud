
#pragma once

#include <lmud/defs.h>
#include <lmud/lisp/any.h>
#include <lmud/net/selector.h>


struct LMud_Server
{
    struct LMud_Net*      net;

    struct LMud_Server**  prev;
    struct LMud_Server*   next;

    LMud_Socket           fd;
    LMud_Any              startup_func;
};

void LMud_Server_Create(struct LMud_Server* self, struct LMud_Net* net, LMud_Socket fd, LMud_Any startup_function);
void LMud_Server_Destroy(struct LMud_Server* self);

struct LMud_Server* LMud_Server_New(struct LMud_Net* net, LMud_Socket fd, LMud_Any startup_function);
void                LMud_Server_Delete(struct LMud_Server* self);

void LMud_Server_Link(struct LMud_Server* self, struct LMud_Server** list);
void LMud_Server_Unlink(struct LMud_Server* self);

LMud_Any LMud_Server_GetStartupFunction(struct LMud_Server* self);

void LMud_Server_RegisterOnSelector(struct LMud_Server* self, struct LMud_Selector* selector);


struct LMud_Servers
{
    struct LMud_Net*     net;
    struct LMud_Server*  servers;
};

void LMud_Servers_Create(struct LMud_Servers* self, struct LMud_Net* net);
void LMud_Servers_Destroy(struct LMud_Servers* self);

void LMud_Servers_Mark(struct LMud_GC* gc, struct LMud_Servers* self);

bool LMud_Servers_OpenV4(struct LMud_Servers* self, const char* address, LMud_Port port, LMud_Any startup_function);
bool LMud_Servers_OpenV6(struct LMud_Servers* self, const char* address, LMud_Port port, LMud_Any startup_function);

void LMud_Servers_RegisterOnSelector(struct LMud_Servers* self, struct LMud_Selector* selector);

void LMud_Servers_Tick(struct LMud_Servers* self, struct LMud_Selector* selector);
