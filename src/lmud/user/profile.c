
#include <lmud/util/memory.h>

#include "profile.h"


void LMud_ProfileRef_Create(struct LMud_ProfileRef* self, struct LMud_Profile* profile)
{
    self->profile = profile;
    self->prev    = NULL;
    self->next    = NULL;

    if (profile != NULL)
    {
        LMud_ProfileRef_Link(self, &profile->references);
    }
}

void LMud_ProfileRef_Destroy(struct LMud_ProfileRef* self)
{
    LMud_ProfileRef_Unlink(self);
}

void LMud_ProfileRef_Link(struct LMud_ProfileRef* self, struct LMud_ProfileRef** list)
{
    LMud_ProfileRef_Unlink(self);

    self->prev =  list;
    self->next = *list;

    if (*list != NULL)
        (*list)->prev = &self->next;

    *list = self;
}

void LMud_ProfileRef_Unlink(struct LMud_ProfileRef* self)
{
    if (self->prev != NULL)
        *self->prev = self->next;

    if (self->next != NULL)
        self->next->prev = self->prev;

    self->prev = NULL;
    self->next = NULL;
}

struct LMud_Profile* LMud_ProfileRef_GetProfile(struct LMud_ProfileRef* self)
{
    return self->profile;
}

void LMud_ProfileRef_SetProfile(struct LMud_ProfileRef* self, struct LMud_Profile* profile)
{
    if (profile != self->profile)
    {
        LMud_ProfileRef_Unlink(self);

        self->profile = profile;

        if (profile != NULL)
        {
            LMud_ProfileRef_Link(self, &profile->references);
        }
    }
}


void LMud_Profile_Create(struct LMud_Profile* self, struct LMud_Profiles* profiles, const char* name)
{
    self->profiles   = profiles;
    self->references = NULL;
    self->name       = LMud_Strdup(name);
}

void LMud_Profile_Destroy(struct LMud_Profile* self)
{
    assert(self->references == NULL);
    LMud_Profile_Unlink(self);
}

void LMud_Profile_Link(struct LMud_Profile* self, struct LMud_Profile** list)
{
    LMud_Profile_Unlink(self);

    self->prev =  list;
    self->next = *list;

    if (*list != NULL)
        (*list)->prev = &self->next;

    *list = self;
}

void LMud_Profile_Unlink(struct LMud_Profile* self)
{
    if (self->prev != NULL)
        *self->prev = self->next;

    if (self->next != NULL)
        self->next->prev = self->prev;

    self->prev = NULL;
    self->next = NULL;
}

const char* LMud_Profile_GetName(struct LMud_Profile* self)
{
    return self->name;
}


static struct LMud_Profile* LMud_Profiles_CreateNew(struct LMud_Profiles* self, const char* name)
{
    struct LMud_Profile* profile;
    
    profile = LMud_Alloc(sizeof(struct LMud_Profile));
    
    if (profile != NULL)
    {
        LMud_Profile_Create(profile, self, name);
        LMud_Profile_Link(profile, &self->profiles);
    }
    
    return profile;
}

static void LMud_Profiles_DeleteProfile(struct LMud_Profiles* self, struct LMud_Profile* profile)
{
    (void) self;
    LMud_Profile_Destroy(profile);
    LMud_Free(profile);
}

bool LMud_Profiles_Create(struct LMud_Profiles* self)
{
    self->profiles = NULL;

    LMud_ProfileRef_Create(&self->system_profile, LMud_Profiles_FindOrCreate(self, "!SYSTEM"));
    LMud_ProfileRef_Create(&self->unprivileged_profile, LMud_Profiles_FindOrCreate(self, "!UNPRIVILEGED"));

    return true;
}

void LMud_Profiles_Destroy(struct LMud_Profiles* self)
{
    LMud_ProfileRef_Destroy(&self->system_profile);
    LMud_ProfileRef_Destroy(&self->unprivileged_profile);

    while (self->profiles != NULL)
    {
        LMud_Profiles_DeleteProfile(self, self->profiles);
    }
}

struct LMud_Profile* LMud_Profiles_GetSystemProfile(struct LMud_Profiles* self)
{
    return LMud_ProfileRef_GetProfile(&self->system_profile);
}

struct LMud_Profile* LMud_Profiles_GetUnprivilegedProfile(struct LMud_Profiles* self)
{
    return LMud_ProfileRef_GetProfile(&self->unprivileged_profile);
}

struct LMud_Profile* LMud_Profiles_Find(struct LMud_Profiles* self, const char* name)
{
    struct LMud_Profile* profile = self->profiles;

    while (profile != NULL)
    {
        if (LMud_CStr_Equals(profile->name, name))
            return profile;

        profile = profile->next;
    }

    return NULL;
}

struct LMud_Profile* LMud_Profiles_FindOrCreate(struct LMud_Profiles* self, const char* name)
{
    struct LMud_Profile* profile = LMud_Profiles_Find(self, name);

    if (profile == NULL)
    {
        profile = LMud_Profiles_CreateNew(self, name);
    }

    return profile;
}
