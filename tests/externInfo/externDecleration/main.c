
#include <stdio.h>

#include "test.h"

extern int i;
extern int j=0;

int main() {
  one();
  two();
  printf( "%d\n", i );
  j++;
  printf( "%d\n", j );
}
