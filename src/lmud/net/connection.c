
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
