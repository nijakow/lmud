
#pragma once

#include <lmud/lisp/base.h>
#include <lmud/util/stream.h>

void LMud_Lisp_Print(struct LMud_Lisp* lisp, LMud_Any object, struct LMud_OutputStream* stream, bool escaped);
void LMud_Lisp_PrintToFile(struct LMud_Lisp* lisp, LMud_Any object, FILE* file, bool escaped);

bool LMud_Lisp_Read(struct LMud_Lisp* lisp, struct LMud_InputStream* stream, LMud_Any* result);
