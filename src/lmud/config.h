
#pragma once

#define LMud_VERSION "0.1"
#define LMud_VERSION_EXTRA "alpha"
#define LMud_RELEASE_NAME "Patchy Phoenix"

#define LMud_ENABLE_COMPRESSED_ANYS

#ifdef __GLIBC__
#  define LMud_ENABLE_MALLOC_TRIM
#endif

#define LMud_UNWIND_PROTECT_UNDEFINED   0xffff
#define LMud_UNWIND_PROTECT_MAX_NESTING 8
