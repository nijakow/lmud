
#include <lmud/util/memory.h>
#include <lmud/util/utf8.h>

#include "port.h"

void LMud_Port_Create(struct LMud_Port* self, struct LMud_Connection* connection)
{
    LMud_ConnectionRef_Create(&self->connection, connection);

    self->pushbacks        = NULL;
    self->pushbacks_alloc  = 0;
    self->pushbacks_fill   = 0;
}

void LMud_Port_Destroy(struct LMud_Port* self)
{
    LMud_ConnectionRef_Destroy(&self->connection);
}

void LMud_Port_Mark(struct LMud_GC* gc, struct LMud_Port* self)
{
    (void) gc;
    (void) self;
}

LMud_Size LMud_Port_CalculateSizeInBytes(struct LMud_Port* self)
{
    (void) self;
    return sizeof(struct LMud_Port) + self->pushbacks_alloc;
}

bool LMud_Port_PushbackByte(struct LMud_Port* self, char byte)
{
    if (self->pushbacks_fill == self->pushbacks_alloc)
    {
        self->pushbacks_alloc += 16;
        self->pushbacks = LMud_Realloc(self->pushbacks, self->pushbacks_alloc);
    }

    self->pushbacks[self->pushbacks_fill++] = byte;

    return true;
}

static bool LMud_Port_PushbackBytesInReverse(struct LMud_Port* self, const char* bytes, LMud_Size size)
{
    while (size --> 0)
    {
        LMud_Port_PushbackByte(self, bytes[size]);
    }

    return true;
}

bool LMud_Port_PushbackRune(struct LMud_Port* self, LMud_Rune rune)
{
    struct LMud_Utf8_Encoder  encoder;
    LMud_Size                 size;

    LMud_Utf8_Encoder_Create(&encoder, rune);
    size = LMud_Utf8_Encoder_RemainingBytes(&encoder);
    char buffer[size];
    LMud_Utf8_Encoder_ReadAll(&encoder, buffer, size);
    LMud_Utf8_Encoder_Destroy(&encoder);
    return LMud_Port_PushbackBytesInReverse(self, buffer, size);
}

bool LMud_Port_WriteByte(struct LMud_Port* self, char byte)
{
    struct LMud_Connection*  connection;

    connection = self->connection.connection;
    
    if (connection == NULL)
        return false;

    return LMud_Connection_WriteByte(connection, byte);
}

bool LMud_Port_FiberReadByte(struct LMud_Port* self, struct LMud_Fiber* fiber)
{
    struct LMud_Connection*  connection;

    connection = self->connection.connection;

    if (connection == NULL)
    {
        // TODO: Error handling.
        return false;
    }

    LMud_Connection_FiberReadByte(connection, fiber);

    return true;
}
