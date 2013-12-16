#include<pthread.h>
#include<stdbool.h>
#include<assert.h>

extern int unknown();

void* fun(void* args){
  int k=0;
  assert(k<1); // succeeds
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, fun, NULL);pthread_join (id, NULL);
  return 0;
}

