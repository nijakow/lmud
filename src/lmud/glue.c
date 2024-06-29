
#include "glue.h"

void LMud_NotifyIncomingConnection(struct LMud* self, struct LMud_Connection* connection)
{
    LMud_Lisp_KickstartNewConnectionTask(&self->lisp, connection);
}
