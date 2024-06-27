
#include <lmud/util/memory.h>

#include "connection.h"


void LMud_Connection_Create(struct LMud_Connection* self, int fd)
{
    self->fd   = fd;
    self->prev = NULL;
    self->next = NULL;
}

void LMud_Connection_Destroy(struct LMud_Connection* self)
{
    LMud_Connection_Unlink(self);
}

void LMud_Connection_Link(struct LMud_Connection* self, struct LMud_Connection** list)
{
    self->prev =  list;
    self->next = *list;
    if (*list != NULL)
        (*list)->prev = &self->next;
    *list = self;
}

void LMud_Connection_Unlink(struct LMud_Connection* self)
{
    if (self->prev != NULL)
        *self->prev = self->next;
    if (self->next != NULL)
        self->next->prev = self->prev;
}

void LMud_Connection_RegisterOnSelector(struct LMud_Connection* self, struct LMud_Selector* selector)
{
    LMud_Selector_AddRead(selector, self->fd);
    LMud_Selector_AddExcept(selector, self->fd);
}


static void LMud_Connections_DeleteConnection(struct LMud_Connections* self, struct LMud_Connection* connection)
{
    (void) self;
    LMud_Connection_Destroy(connection);
    LMud_Free(connection);
}

void LMud_Connections_Create(struct LMud_Connections* self)
{
    self->connections = NULL;
}

void LMud_Connections_Destroy(struct LMud_Connections* self)
{
    while (self->connections != NULL)
    {
        LMud_Connections_DeleteConnection(self, self->connections);
    }
}

void LMud_Connections_RegisterOnSelector(struct LMud_Connections* self, struct LMud_Selector* selector)
{
    struct LMud_Connection*  connection;

    for (connection = self->connections; connection != NULL; connection = connection->next)
    {
        LMud_Connection_RegisterOnSelector(connection, selector);
    }
}
