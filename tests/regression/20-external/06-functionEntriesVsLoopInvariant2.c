
#include <stdio.h>
#include <assert.h>

void f( int i ){
  while (i) {
    assert(i==5); // SUCCESS, because of wrong invariant ( should be a function-entry invariant, but is not )
    i--;
  }
  // dead-code here!
}

void main(){
  f(5);
  // dead-code here!
}

