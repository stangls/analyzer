#include "dry.h"

#ifdef  NOSTRUCTASSIGN
void memcpy_x(char *d, char *s, int l)
{
        while (l--)
          *d++ = *s++; 
        counter ++;
}
#endif
