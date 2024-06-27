
#include <lmud/lmud.h>

struct LMud  LMUD;


static void LMud_SignalHandler(int signal)
{
    LMud_SignalInterrupt(&LMUD, signal);
}

static void LMud_InstallSignalHandlers()
{
    signal(SIGINT,  LMud_SignalHandler);
    signal(SIGTERM, LMud_SignalHandler);
}


int main(int argc, char* argv[])
{
    if (LMud_Create(&LMUD))
    {
        LMud_InstallSignalHandlers();
        LMud_Main(&LMUD, argc, argv);
        LMud_Destroy(&LMUD);
    }
    return 0;
}
