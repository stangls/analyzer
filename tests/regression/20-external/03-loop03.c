#include<stdio.h>
#include<assert.h>

/*
  this file has an according .ix file with external information
*/

int main () {
  int i,j,k;

  i = j = k = 0;
  while (i < 10) {
    i++;
    j=i;
    assert(j <= 10*2); // UNKNOWN
    assert(j>=0); // UNKNOWN
    assert(j == i); // UNKNOWN
  }
  return 0;
}

