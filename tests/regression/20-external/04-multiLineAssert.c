
#include <assert.h>
#include <stdio.h>

int i=0;

int f( int i ){
  return i+1;
}

void main() {
  i=f(f(0))+f(1);
  i++;
  /*
    if the invariant at line 10 (see .ix file) would be required
    after returning from f, we would have deadcode here.
  */
  assert(1); // SUCCESS
  assert(i==5); // SUCCESS
  printf("i is %d\n",i);
}

