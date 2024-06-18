
#include "symbol.h"

void LMud_Symbol_Create(struct LMud_Symbol* self, struct LMud_Symbol** list, LMud_Any name)
{
    self->prev = NULL;
    self->next = NULL;

    self->name = name;

    // TODO: value, function

    LMud_Symbol_Link(self, list);
}

void LMud_Symbol_Destroy(struct LMud_Symbol* self)
{
    LMud_Symbol_Unlink(self);
}


void LMud_Symbol_Unlink(struct LMud_Symbol* self)
{
    if (self->next != NULL)
        self->next->prev = self->prev;
    if (self->prev != NULL)
        *self->prev = self->next;

    self->prev = NULL;
    self->next = NULL;
}

void LMud_Symbol_Link(struct LMud_Symbol* self, struct LMud_Symbol** list)
{
    LMud_Symbol_Unlink(self);

    self->prev =  list;
    self->next = *list;

    if (*list != NULL)
    {
        (*list)->prev = &self->next;
    }

    *list = self;
}
