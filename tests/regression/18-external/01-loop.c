#include<stdio.h>
#include<assert.h>

/*
  this file has an according .ix file with external information
*/

int main () {
  int i,j,k;

  i = k = 0; j = 7;
  while (i < 10) {
    i++;
    j = 7;
    k = 5;
  }
  assert(i == 10); // SUCCESS
  assert(k);  // SUCCESS
  assert(j==7);
  return 0;
}

