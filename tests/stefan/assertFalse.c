#include<pthread.h>
#include<stdbool.h>
#include<assert.h>

void fun2(int x){
  assert(x);
}

void* fun(void* args){
  int y=1;
  fun2(y);
  y--;
  fun2(y);
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, fun, NULL);pthread_join (id, NULL);
  return 0;
}

