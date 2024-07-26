/**
 * @file inet.h
 * @brief The LMud Networking Utilities Header
 * 
 * This file contains networking utilities for the LMud project.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#pragma once

#include <lmud/defs.h>

bool LMud_Inet_OpenServerV4(const char* address, LMud_Port port, LMud_Socket* socket);
bool LMud_Inet_OpenServerV6(const char* address, LMud_Port port, LMud_Socket* socket);


struct LMud_Inet_AcceptInfo
{
    LMud_Socket              socket;
    struct sockaddr_storage  addr;
    socklen_t                addrlen;
};

void LMud_Inet_AcceptInfo_Create(struct LMud_Inet_AcceptInfo* self);
void LMud_Inet_AcceptInfo_Destroy(struct LMud_Inet_AcceptInfo* self);

LMud_Socket LMud_Inet_AcceptInfo_GetSocket(struct LMud_Inet_AcceptInfo* self);

bool LMud_Inet_Accept(LMud_Socket socket, struct LMud_Inet_AcceptInfo* info);


void LMud_Inet_Close(LMud_Socket socket);
