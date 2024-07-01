
#include "glue.h"

void LMud_NotifyIncomingConnection(struct LMud* self, LMud_Any startup_function, struct LMud_Connection* connection)
{
    LMud_Lisp_KickstartNewConnectionTask(&self->lisp, startup_function, connection);
}

void LMud_Mark(struct LMud_GC* gc, struct LMud* self)
{
    LMud_Lisp_Mark(gc, &self->lisp);
    LMud_Net_Mark(gc, &self->net);
}
