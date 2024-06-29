
#include "port.h"

void LMud_Port_Create(struct LMud_Port* self, struct LMud_Connection* connection)
{
    LMud_ConnectionRef_Create(&self->connection, connection);
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
    return sizeof(struct LMud_Port);
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
