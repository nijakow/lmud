
#include <lmud/lmud.h>

struct LMud  LMUD;


int main(int argc, char* argv[])
{
    if (LMud_Create(&LMUD))
    {
        LMud_Main(&LMUD, argc, argv);
        LMud_Destroy(&LMUD);
    }
    return 0;
}
