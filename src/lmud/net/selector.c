
#include "selector.h"

void LMud_Selector_Create(struct LMud_Selector* self)
{
    FD_ZERO(&self->read_fds);
    FD_ZERO(&self->write_fds);
    FD_ZERO(&self->except_fds);
    self->max_fd = 0;
}

void LMud_Selector_Destroy(struct LMud_Selector* self)
{
    (void)  self;
}

static void LMud_Selector_UpdateMax(struct LMud_Selector* self, int fd)
{
    if (fd > self->max_fd)
        self->max_fd = fd;
}

void LMud_Selector_AddRead(struct LMud_Selector* self, int fd)
{
    FD_SET(fd, &self->read_fds);
    LMud_Selector_UpdateMax(self, fd);
}

void LMud_Selector_AddWrite(struct LMud_Selector* self, int fd)
{
    FD_SET(fd, &self->write_fds);
    LMud_Selector_UpdateMax(self, fd);
}

void LMud_Selector_AddExcept(struct LMud_Selector* self, int fd)
{
    FD_SET(fd, &self->except_fds);
    LMud_Selector_UpdateMax(self, fd);
}

bool LMud_Selector_IsRead(struct LMud_Selector* self, int fd)
{
    return FD_ISSET(fd, &self->read_fds);
}

bool LMud_Selector_IsWrite(struct LMud_Selector* self, int fd)
{
    return FD_ISSET(fd, &self->write_fds);
}

bool LMud_Selector_IsExcept(struct LMud_Selector* self, int fd)
{
    return FD_ISSET(fd, &self->except_fds);
}

void LMud_Selector_Select(struct LMud_Selector* self, bool block)
{
    struct timeval   timeout;
    struct timeval*  timeout_ptr;

    if (block)
        timeout_ptr = NULL;
    else {
        timeout.tv_sec  = 0;
        timeout.tv_usec = 0;
        timeout_ptr = &timeout;
    }

    select(self->max_fd + 1, &self->read_fds, &self->write_fds, &self->except_fds, timeout_ptr);
}
