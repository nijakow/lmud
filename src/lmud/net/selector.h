/**
 * @file selector.h
 * @brief Wrappers around the select system call
 * 
 * This file contains wrappers around the select system call for the LMud project.
 * 
 * @copyright Copyright (c) 2024 Eric Felix Nijakowski
 * 
 * @license See LICENSE file for details.
 */

#pragma once

#include <lmud/defs.h>

struct LMud_Selector
{
    fd_set  read_fds;
    fd_set  write_fds;
    fd_set  except_fds;
    int     max_fd;
};

void LMud_Selector_Create(struct LMud_Selector* self);
void LMud_Selector_Destroy(struct LMud_Selector* self);

void LMud_Selector_AddRead(struct LMud_Selector* self, int fd);
void LMud_Selector_AddWrite(struct LMud_Selector* self, int fd);
void LMud_Selector_AddExcept(struct LMud_Selector* self, int fd);

bool LMud_Selector_IsRead(struct LMud_Selector* self, int fd);
bool LMud_Selector_IsWrite(struct LMud_Selector* self, int fd);
bool LMud_Selector_IsExcept(struct LMud_Selector* self, int fd);

void LMud_Selector_Select(struct LMud_Selector* self, bool block);
