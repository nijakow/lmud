
#include "package.h"


void LMud_Package_Create(struct LMud_Package* self, LMud_Any name)
{
    self->name = name;
    LMud_SymbolTable_Create(&self->symbols);
}

void LMud_Package_Destroy(struct LMud_Package* self)
{
    LMud_SymbolTable_Destroy(&self->symbols);
}
