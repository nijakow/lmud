
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
