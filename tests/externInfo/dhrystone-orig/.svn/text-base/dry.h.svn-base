extern int counter;
static char feld[0x0300];
static int zz;

int strcmp_x( char *s, char *t );
void strcpy_x( char s[], char t[] );
char  *malloc_x(int size);

/* Accuracy of timings and human fatigue controlled by next two lines */
#define LOOPS   1            /* Use this for slow or 16 bit machines */
/*#define LOOPS 50000          /* Use this for faster machines */

/* Compiler dependent options */
#undef  NOENUM                  /* Define if compiler has no enum's */
#define  NOSTRUCTASSIGN          /* Define if compiler can't assign structures */

/* define only one of the next two defines */
/*#define TIMES                   /* Use times(2) time function */
/*#define TIME                  /* Use time(2) time function */

/* define the granularity of your times(2) function (when used) */
/*#define HZ      60             /* times(2) returns 1/60 second (most) */
#define HZ    100             /* times(2) returns 1/100 second (WECo) */

/* for compatibility with goofed up version */
/*#define GOOF                  /* Define if you want the goofed up version */

extern char Version[];

#ifdef  NOSTRUCTASSIGN
void memcpy_x(char *d, char *s, int l);
#endif

#ifdef  NOSTRUCTASSIGN
#define structassign(d, s)      memcpy_x(&(d), &(s), sizeof(d))
#else
#define structassign(d, s)      d = s
#endif

#ifdef  NOENUM
#define Ident1  1
#define Ident2  2
#define Ident3  3
#define Ident4  4
#define Ident5  5
typedef int     Enumeration;
#else
typedef enum    {Ident1, Ident2, Ident3, Ident4, Ident5} Enumeration;
#endif

typedef int     OneToThirty;
typedef int     OneToFifty;
typedef char    CapitalLetter;
typedef char    String30[31];
typedef int     Array1Dim[51];
typedef int     Array2Dim[51][51];

struct  Record
{
        struct Record           *PtrComp;
        Enumeration             Discr;
        Enumeration             EnumComp;
        OneToFifty              IntComp;
        String30                StringComp;
};

typedef struct Record   RecordType;
typedef RecordType *    RecordPtr;
typedef int             boolean;

#define NULL            0
#define TRUE            1
#define FALSE           0

#ifndef REG
#define REG
#endif

#ifdef TIMES
#include <sys/types.h>
#include <sys/times.h>
#endif


void Proc0();
void Proc1(REG RecordPtr PtrParIn);
void Proc2(OneToFifty *IntParIO);
void Proc3(RecordPtr *PtrParOut);
void Proc4();
void Proc5();
void Proc6(REG Enumeration EnumParIn, REG Enumeration *EnumParOut);
void Proc7(OneToFifty IntParI1, OneToFifty IntParI2, OneToFifty *IntParOut);
void Proc8(Array1Dim Array1Par, Array2Dim Array2Par, OneToFifty IntParI1, OneToFifty IntParI2);
Enumeration Func1(CapitalLetter CharPar1, CapitalLetter CharPar2);
boolean Func2(String30 StrParI1, String30 StrParI2);
boolean Func3(REG Enumeration EnumParIn);

extern int             IntGlob;
extern boolean         BoolGlob;
extern char            Char1Glob;
extern char            Char2Glob;
extern Array1Dim       Array1Glob;
extern Array2Dim       Array2Glob;
extern RecordPtr       PtrGlb;
extern RecordPtr       PtrGlbNext;
extern volatile int    SIG_IN;
