
#pragma once

#include <lmud/defs.h>

bool LMud_Inet_OpenServerV4(const char* address, LMud_Port port, int* fd);
bool LMud_Inet_OpenServerV6(const char* address, LMud_Port port, int* fd);

struct LMud_Inet_AcceptInfo
{
    int                      fd;
    struct sockaddr_storage  addr;
    socklen_t                addrlen;
};

void LMud_Inet_AcceptInfo_Create(struct LMud_Inet_AcceptInfo* self);
void LMud_Inet_AcceptInfo_Destroy(struct LMud_Inet_AcceptInfo* self);

bool LMud_Inet_Accept(int fd, struct LMud_Inet_AcceptInfo* info);

void LMud_Inet_Close(int fd);
