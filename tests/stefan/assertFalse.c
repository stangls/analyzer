#include<pthread.h>
#include<stdbool.h>
#include<assert.h>

void* fun(void* args){
  assert(0);
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, fun, NULL);pthread_join (id, NULL);

















































  return 0;
}

