
#pragma once

#include <lmud/defs.h>
#include <lmud/decls.h>


struct LMud_ProfileRef
{
    struct LMud_Profile*      profile;
    struct LMud_ProfileRef**  prev;
    struct LMud_ProfileRef*   next;
};

void LMud_ProfileRef_Create(struct LMud_ProfileRef* self, struct LMud_Profile* profile);
void LMud_ProfileRef_Destroy(struct LMud_ProfileRef* self);

void LMud_ProfileRef_Link(struct LMud_ProfileRef* self, struct LMud_ProfileRef** list);
void LMud_ProfileRef_Unlink(struct LMud_ProfileRef* self);

struct LMud_Profile* LMud_ProfileRef_GetProfile(struct LMud_ProfileRef* self);


struct LMud_Profile
{
    struct LMud_Profiles*    profiles;
    struct LMud_ProfileRef*  references;

    struct LMud_Profile**    prev;
    struct LMud_Profile*     next;

    char*                    name;
};

void LMud_Profile_Create(struct LMud_Profile* self, struct LMud_Profiles* profiles, const char* name);
void LMud_Profile_Destroy(struct LMud_Profile* self);

void LMud_Profile_Link(struct LMud_Profile* self, struct LMud_Profile** list);
void LMud_Profile_Unlink(struct LMud_Profile* self);

const char* LMud_Profile_GetName(struct LMud_Profile* self);


struct LMud_Profiles
{
    struct LMud_Profile*    profiles;

    struct LMud_ProfileRef  system_profile;
    struct LMud_ProfileRef  unprivileged_profile;
};

bool LMud_Profiles_Create(struct LMud_Profiles* self);
void LMud_Profiles_Destroy(struct LMud_Profiles* self);

struct LMud_Profile* LMud_Profiles_GetSystemProfile(struct LMud_Profiles* self);
struct LMud_Profile* LMud_Profiles_GetUnprivilegedProfile(struct LMud_Profiles* self);

struct LMud_Profile* LMud_Profiles_Find(struct LMud_Profiles* self, const char* name);
struct LMud_Profile* LMud_Profiles_FindOrCreate(struct LMud_Profiles* self, const char* name);
