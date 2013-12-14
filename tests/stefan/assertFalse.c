#include<pthread.h>
#include<stdbool.h>
#include<assert.h>

  void* fun(void* args){
    int x = 0;
    assert(x);
    return NULL;
  }

  int main() {
    pthread_t id;
    pthread_create(&id, NULL, fun, NULL);pthread_join (id, NULL);
    return 0;
  }

