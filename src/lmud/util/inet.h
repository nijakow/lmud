
#pragma once

#include <lmud/defs.h>

bool LMud_Inet_OpenServerV4(const char* address, LMud_Port port, int* fd);
bool LMud_Inet_OpenServerV6(const char* address, LMud_Port port, int* fd);
void LMud_Inet_Close(int fd);
