
#pragma once

#include <lmud/defs.h>

struct LMud
{
};

bool LMud_Create(struct LMud* self);
void LMud_Destroy(struct LMud* self);

void LMud_Main(struct LMud* self, int argc, char* argv[]);
