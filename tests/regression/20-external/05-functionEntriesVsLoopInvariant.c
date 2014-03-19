
#include <stdio.h>
#include <assert.h>

void f( int i ){
  while (i)
    i--;
  // no dead-code here!
  assert(1); // SUCCESS
}

void main(){
  f(5);
  // no dead-code here!
  assert(1); // SUCCESS
}

