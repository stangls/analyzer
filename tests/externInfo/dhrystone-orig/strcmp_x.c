#include "dry.h"

int strcmp_x( char *s, char *t ) 
{
        for ( ; *s == *t; s++, t++ , counter++)
                if ( *s == '\0' )
                        return( 0 ) ;
        return( *s - *t ) ;
}
