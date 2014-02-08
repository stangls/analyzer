# 1 "src/Proc6.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "src/Proc6.c"
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
# 2 "src/Proc6.c" 2

void Proc6( Enumeration EnumParIn, Enumeration *EnumParOut)
{
        *EnumParOut = EnumParIn;
        if (! Func3(EnumParIn) ){
                *EnumParOut = Ident4;
                counter+=43;
        }
        switch (EnumParIn)
        {
        case Ident1: counter+=5; *EnumParOut = Ident1; break;
        case Ident2: counter+=6; if (IntGlob > 100) *EnumParOut = Ident1;
                        else *EnumParOut = Ident4;
                        break;
        case Ident3: counter+=7; *EnumParOut = Ident2; break;
        case Ident4: counter+=8; break;
        case Ident5: counter+=9; *EnumParOut = Ident3;
        }
}
