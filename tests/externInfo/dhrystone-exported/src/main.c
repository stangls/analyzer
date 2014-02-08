#include "dry.h"

#ifdef GOOF
char Version[] = "1.0";
#else	/* */
char Version[] = "1.1";
#endif	/* */

int counter;
extern char feld[0x0300];
extern int zz;
int IntGlob;
boolean BoolGlob;
char Char1Glob;
char Char2Glob;
Array1Dim Array1Glob;
Array2Dim Array2Glob;
RecordPtr PtrGlb;
RecordPtr PtrGlbNext;
volatile int SIG_IN;

main ()
{
  counter = 7;
  zz = 0;
  Proc0 ();
  return counter;
}


