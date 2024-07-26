/**
 * @file glue.h
 * @brief The LMud Kernel Glue Code Header
 * 
 * This file contains the glue code for the LMud project.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#pragma once

#include <lmud/defs.h>
#include <lmud/lmud.h>

#include <lmud/lisp/any.h>
#include <lmud/net/connection.h>

struct LMud_Log*      LMud_GetLog(struct LMud* self);
struct LMud_Profiles* LMud_GetProfiles(struct LMud* self);
struct LMud_Lisp*     LMud_GetLisp(struct LMud* self);
struct LMud_Net*      LMud_GetNet(struct LMud* self);
struct timeval*       LMud_GetStartTime(struct LMud* self);

void LMud_NotifyIncomingConnection(struct LMud* self, LMud_Any startup_function, struct LMud_Connection* connection);

void LMud_Mark(struct LMud_GC* gc, struct LMud* self);
