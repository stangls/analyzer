#include "fun.h"
#include <stdio.h>

/* multiple function calls in one location */

int main(){
  one();
  fun(one());
  fun(two());
  int x = fun(fun(1));
  printf("%d",fun(x));
  return 0;
}
