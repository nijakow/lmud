/**
 * @file defs.h
 * @brief LMud Definitions
 * 
 * This file contains the root definitions for the LMud project.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

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

#include <errno.h>
#include <ctype.h>
#include <fcntl.h>
#include <signal.h>
#include <time.h>
#include <sys/time.h>

// Networking stuff
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <lmud/config.h>

#ifdef LMud_FEATURE_CONSOLE
# include <sys/ioctl.h>
# include <termios.h>
#endif


typedef size_t        LMud_Size;
typedef int           LMud_Integer;
typedef unsigned int  LMud_Rune;

typedef uint16_t      LMud_Port;
typedef int           LMud_Socket;


enum LMud_Runes
{
    LMud_Rune_Left  = 0xf0000000,
    LMud_Rune_Right = 0xf0000001,
    LMud_Rune_Up    = 0xf0000002,
    LMud_Rune_Down  = 0xf0000003
};
