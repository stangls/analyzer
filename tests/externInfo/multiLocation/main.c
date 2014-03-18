#include "fun.h"
#include <stdio.h>

/* multiple function calls in one location */

int main(){
  one();
  fun(one());
  fun(two());
  printf("%d",1);
  return 0;
}
