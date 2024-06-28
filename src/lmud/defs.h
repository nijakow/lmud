
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

#include <signal.h>

// Networking stuff
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netinet/in.h>
#include <arpa/inet.h>


#define LMud_VERSION "0.1"
#define LMud_VERSION_EXTRA "alpha"
#define LMud_RELEASE_NAME "Patchy Phoenix"

#define LMud_ENABLE_COMPRESSED_ANYS
#define LMud_ENABLE_MALLOC_TRIM

#define LMud_UNWIND_PROTECT_UNDEFINED   0xffff
#define LMud_UNWIND_PROTECT_MAX_NESTING 8

typedef size_t        LMud_Size;
typedef int           LMud_Integer;
typedef unsigned int  LMud_Rune;

typedef uint16_t      LMud_Port;
typedef int           LMud_Socket;
