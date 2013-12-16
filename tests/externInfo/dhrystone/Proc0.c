# 1 "src/Proc0.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "src/Proc0.c"
# 1 "src/dry.h" 1
extern int counter;
static char feld[0x0300];
static int zz;

int strcmp_x( char *s, char *t );
void strcpy_x( char s[], char t[] );
char *malloc_x(int size);
# 28 "src/dry.h"
extern char Version[];


void memcpy_x(char *d, char *s, int l);
# 48 "src/dry.h"
typedef enum {Ident1, Ident2, Ident3, Ident4, Ident5} Enumeration;


typedef int OneToThirty;
typedef int OneToFifty;
typedef char CapitalLetter;
typedef char String30[31];
typedef int Array1Dim[51];
typedef int Array2Dim[51][51];

struct Record
{
        struct Record *PtrComp;
        Enumeration Discr;
        Enumeration EnumComp;
        OneToFifty IntComp;
        String30 StringComp;
};

typedef struct Record RecordType;
typedef RecordType * RecordPtr;
typedef int boolean;
# 85 "src/dry.h"
void Proc0();
void Proc1( RecordPtr PtrParIn);
void Proc2(OneToFifty *IntParIO);
void Proc3(RecordPtr *PtrParOut);
void Proc4();
void Proc5();
void Proc6( Enumeration EnumParIn, Enumeration *EnumParOut);
void Proc7(OneToFifty IntParI1, OneToFifty IntParI2, OneToFifty *IntParOut);
void Proc8(Array1Dim Array1Par, Array2Dim Array2Par, OneToFifty IntParI1, OneToFifty IntParI2);
Enumeration Func1(CapitalLetter CharPar1, CapitalLetter CharPar2);
boolean Func2(String30 StrParI1, String30 StrParI2);
boolean Func3( Enumeration EnumParIn);

extern int IntGlob;
extern boolean BoolGlob;
extern char Char1Glob;
extern char Char2Glob;
extern Array1Dim Array1Glob;
extern Array2Dim Array2Glob;
extern RecordPtr PtrGlb;
extern RecordPtr PtrGlbNext;
extern volatile int SIG_IN;
# 2 "src/Proc0.c" 2

void Proc0()
{
        OneToFifty IntLoc1;
        OneToFifty IntLoc2;
        OneToFifty IntLoc3;
        char CharLoc;
        char CharIndex;
        Enumeration EnumLoc;
        String30 String1Loc;
        String30 String2Loc;


        unsigned int i;
# 39 "src/Proc0.c"
        PtrGlbNext = (RecordPtr) malloc_x(sizeof(RecordType));
        PtrGlb = (RecordPtr) malloc_x(sizeof(RecordType));
        PtrGlb->PtrComp = PtrGlbNext;
        PtrGlb->Discr = Ident1;
        PtrGlb->EnumComp = Ident3;
        PtrGlb->IntComp = 40;
        strcpy_x(PtrGlb->StringComp, "DHRYSTONE PROGRAM, SOME STRING");

        strcpy_x(String1Loc, "DHRYSTONE PROGRAM, 1'ST STRING");

        Array2Glob[8][7] = 10;
# 60 "src/Proc0.c"
        for (i = 0; i < 1; ++i)
        {

                counter+=3;
                Proc5();
                Proc4();
                IntLoc1 = 2;
                IntLoc2 = 3;
                strcpy_x(String2Loc, "DHRYSTONE PROGRAM, 2'ND STRING");
                EnumLoc = Ident2;
                BoolGlob = ! Func2(String1Loc, String2Loc);
                while (IntLoc1 < IntLoc2)
                {
                        IntLoc3 = 5 * IntLoc1 - IntLoc2;
                        Proc7(IntLoc1, IntLoc2, &IntLoc3);
                        counter+=9;
                        ++IntLoc1;
                }
                Proc8(Array1Glob, Array2Glob, IntLoc1, IntLoc3);
                Proc1(PtrGlb);
                for (CharIndex = 'A'; CharIndex <= Char2Glob; ++CharIndex,++counter)
                        if (EnumLoc == Func1(CharIndex, 'C'))
                                Proc6(Ident1, &EnumLoc);
                IntLoc3 = IntLoc2 * IntLoc1;
                IntLoc2 = IntLoc3 / IntLoc1;
                IntLoc2 = 7 * (IntLoc3 - IntLoc2) - IntLoc1;
                Proc2(&IntLoc1);
                counter+=11;
        }
# 112 "src/Proc0.c"
}
