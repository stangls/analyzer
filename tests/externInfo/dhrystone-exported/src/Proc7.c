#include "dry.h"

void Proc7(OneToFifty IntParI1, OneToFifty IntParI2, OneToFifty *IntParOut)
{
        REG OneToFifty  IntLoc;

	if (SIG_IN) {
	  IntLoc = IntParI1/0;
	}
        IntLoc = IntParI1 + 2;
        counter+=IntLoc;
        *IntParOut = IntParI2 + IntLoc;
}
