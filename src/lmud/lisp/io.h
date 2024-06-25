
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/util/stream.h>

void LMud_Lisp_Print(struct LMud_Lisp* lisp, LMud_Any object, FILE* stream, bool escaped);

bool LMud_Lisp_Read(struct LMud_Lisp* lisp, struct LMud_InputStream* stream, LMud_Any* result);
