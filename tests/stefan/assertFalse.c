#include<pthread.h>
#include<stdbool.h>
#include<assert.h>

void fun(int x){
  assert(x);
}

void* fun(void* args){
  assert(1);
  assert(0);
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, fun, NULL);pthread_join (id, NULL);
  return 0;
}

