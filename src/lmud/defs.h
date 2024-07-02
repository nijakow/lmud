
#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include <assert.h>
#include <stdarg.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <ctype.h>
#include <fcntl.h>
#include <signal.h>
#include <time.h>

// Networking stuff
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <lmud/config.h>


typedef size_t        LMud_Size;
typedef int           LMud_Integer;
typedef unsigned int  LMud_Rune;

typedef uint16_t      LMud_Port;
typedef int           LMud_Socket;
