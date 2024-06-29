
#include <lmud/util/memory.h>

#include "ringbuffer.h"

static LMud_Size LMud_Ringbuffer_NextIndex(struct LMud_Ringbuffer* self, LMud_Size index)
{
    return (index + 1) % self->alloc;
}

void LMud_Ringbuffer_Create(struct LMud_Ringbuffer* self, LMud_Size size)
{
    self->data  = LMud_Alloc(size);
    self->alloc = size;
    self->read  = 0;
    self->write = 0;
}

void LMud_Ringbuffer_Destroy(struct LMud_Ringbuffer* self)
{
    LMud_Free(self->data);
}

bool LMud_Ringbuffer_IsFull(struct LMud_Ringbuffer* self)
{
    return LMud_Ringbuffer_NextIndex(self, self->write) == self->read;
}

bool LMud_Ringbuffer_IsEmpty(struct LMud_Ringbuffer* self)
{
    return self->read == self->write;
}

bool LMud_Ringbuffer_HasData(struct LMud_Ringbuffer* self)
{
    return !LMud_Ringbuffer_IsEmpty(self);
}

bool LMud_Ringbuffer_PeekBytes(struct LMud_Ringbuffer* self, char* bytes, LMud_Size size)
{
    LMud_Size  i;
    LMud_Size  read;
    
    read = self->read;

    for (i = 0; i < size; ++i)
    {
        if (read == self->write)
            return false;

        bytes[i] = self->data[read];
        read = LMud_Ringbuffer_NextIndex(self, read);
    }

    return true;
}

bool LMud_Ringbuffer_SkipBytes(struct LMud_Ringbuffer* self, LMud_Size size)
{
    LMud_Size i;

    for (i = 0; i < size; ++i)
    {
        if (self->read == self->write)
            return false;

        self->read = LMud_Ringbuffer_NextIndex(self, self->read);
    }

    return true;
}

bool LMud_Ringbuffer_WriteByte(struct LMud_Ringbuffer* self, char byte)
{
    if (LMud_Ringbuffer_IsFull(self))
        return false;
    else {
        self->data[self->write] = byte;
        self->write = LMud_Ringbuffer_NextIndex(self, self->write);
        return true;
    }
}

bool LMud_Ringbuffer_WriteBytes(struct LMud_Ringbuffer* self, const char* bytes, LMud_Size size)
{
    LMud_Size i;

    for (i = 0; i < size; ++i)
    {
        if (!LMud_Ringbuffer_WriteByte(self, bytes[i]))
            return false;
    }

    return true;
}

bool LMud_Ringbuffer_ReadByte(struct LMud_Ringbuffer* self, char* byte)
{
    if (LMud_Ringbuffer_IsEmpty(self))
        return false;
    else {
        *byte = self->data[self->read];
        self->read = LMud_Ringbuffer_NextIndex(self, self->read);
        return true;
    }
}

LMud_Size LMud_Ringbuffer_ReadBytes(struct LMud_Ringbuffer* self, char* bytes, LMud_Size size)
{
    LMud_Size i;

    for (i = 0; i < size; ++i)
    {
        if (!LMud_Ringbuffer_ReadByte(self, &bytes[i]))
            return i;
    }

    return size;
}
