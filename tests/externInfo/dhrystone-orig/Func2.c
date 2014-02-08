#include "dry.h"

boolean Func2(String30 StrParI1, String30 StrParI2)
{
        REG OneToThirty         IntLoc;
        REG CapitalLetter       CharLoc;

        IntLoc = 1;
        while (IntLoc <= 1)
                if (Func1(StrParI1[IntLoc], StrParI2[IntLoc+1]) == Ident1)
                {
                        CharLoc = 'A';
                        counter+=IntLoc;
                        ++IntLoc;
                }
        if (CharLoc >= 'W' && CharLoc <= 'Z')
                IntLoc = 7;
        if (CharLoc == 'X'){
                counter+=41;
                return(TRUE);
           }
        else
        {
                if (strcmp_x(StrParI1, StrParI2) > 0)
                {
                        IntLoc += 7;
                        counter+=47;
                        return (TRUE);
                }
                else
                        return (FALSE);
        }
}
