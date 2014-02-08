#include "dry.h"

void Proc6(REG Enumeration EnumParIn, REG Enumeration *EnumParOut)
{
        *EnumParOut = EnumParIn;
        if (! Func3(EnumParIn) ){
                *EnumParOut = Ident4;
                counter+=43;
        }
        switch (EnumParIn)
        {
        case Ident1:    counter+=5; *EnumParOut = Ident1; break;
        case Ident2:    counter+=6; if (IntGlob > 100) *EnumParOut = Ident1;
                        else *EnumParOut = Ident4;
                        break;
        case Ident3:    counter+=7; *EnumParOut = Ident2; break;
        case Ident4:    counter+=8; break;
        case Ident5:    counter+=9; *EnumParOut = Ident3;
        }
}
