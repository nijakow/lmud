/**
 * @file glue.c
 * @brief The LMud Kernel Glue Code
 * 
 * This file contains the glue code for the LMud project.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#include "glue.h"

void LMud_NotifyIncomingConnection(struct LMud* self, LMud_Any startup_function, struct LMud_Connection* connection)
{
    LMud_Lisp_KickstartNewConnectionTask(
        &self->lisp,
        LMud_Profiles_GetUnprivilegedProfile(LMud_GetProfiles(self)),
        startup_function,
        connection
    );
}

void LMud_Mark(struct LMud_GC* gc, struct LMud* self)
{
    LMud_Lisp_Mark(gc, &self->lisp);
    LMud_Net_Mark(gc, &self->net);
}


struct LMud_Log* LMud_GetLog(struct LMud* self)
{
    return &self->log;
}

struct LMud_Profiles* LMud_GetProfiles(struct LMud* self)
{
    return &self->profiles;
}

struct LMud_Lisp* LMud_GetLisp(struct LMud* self)
{
    return &self->lisp;
}

struct LMud_Net* LMud_GetNet(struct LMud* self)
{
    return &self->net;
}

struct timeval* LMud_GetStartTime(struct LMud* self)
{
    return &self->start_time;
}

void LMud_Logf(struct LMud* mud, enum LMud_LogLevel loglevel, const char* format, ...)
{
    va_list                  args;
    struct LMud_LogComposer  composer;
    struct LMud_OutputStream stream;

    /*
     * If the log won't print this level, don't bother.
     */
    if (LMud_Log_AcceptsLogLevel(LMud_GetLog(mud), loglevel))
    {
        va_start(args, format);
        {
            LMud_LogComposer_Create(&composer, LMud_GetLog(mud), loglevel);
            LMud_OutputStream_CreateOnLogComposer(&stream, &composer);
            LMud_OutputStream_VPrintf(&stream, format, args);
            LMud_OutputStream_Destroy(&stream);
            LMud_LogComposer_Commit(&composer);
            LMud_LogComposer_Destroy(&composer);
        }
        va_end(args);
    }
}

void LMud_SetStatus(struct LMud* self, const char* status)
{
    LMud_Console_SetStatus(&self->console, status);
}
