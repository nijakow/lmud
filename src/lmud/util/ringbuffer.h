
#pragma once

#include <lmud/defs.h>

struct LMud_Ringbuffer
{
    char*      data;
    LMud_Size  alloc;
    LMud_Size  read;
    LMud_Size  write;
};

void LMud_Ringbuffer_Create(struct LMud_Ringbuffer* self, LMud_Size size);
void LMud_Ringbuffer_Destroy(struct LMud_Ringbuffer* self);

bool LMud_Ringbuffer_IsFull(struct LMud_Ringbuffer* self);
bool LMud_Ringbuffer_IsEmpty(struct LMud_Ringbuffer* self);
bool LMud_Ringbuffer_HasData(struct LMud_Ringbuffer* self);

LMud_Size LMud_Ringbuffer_PeekBytes(struct LMud_Ringbuffer* self, char* bytes, LMud_Size size);
bool      LMud_Ringbuffer_SkipBytes(struct LMud_Ringbuffer* self, LMud_Size size);

bool LMud_Ringbuffer_WriteByte(struct LMud_Ringbuffer* self, char byte);
bool LMud_Ringbuffer_WriteBytes(struct LMud_Ringbuffer* self, const char* bytes, LMud_Size size);

bool      LMud_Ringbuffer_ReadByte(struct LMud_Ringbuffer* self, char* byte);
LMud_Size LMud_Ringbuffer_ReadBytes(struct LMud_Ringbuffer* self, char* bytes, LMud_Size size);
