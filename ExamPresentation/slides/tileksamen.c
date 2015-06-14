#include <pthread.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#define NUM_THREADS 5

void* printWorker(void* input) {
  double *pt = (double *)input;
  printf("%lf\n", *pt);
  //pthread_exit(NULL);
}

void* sqrtWorker(void* input) {
  long* inputLong = (long*) input;
  double resPtr = sqrt(*inputLong);
  pthread_t thread;
  pthread_create(&thread, NULL, printWorker, (void*) &resPtr);
  pthread_exit(NULL);
}

int main(int argc, char *argv[])
{
  pthread_t threads[NUM_THREADS];
  long i;
  long j;
  for(i = 0, j = 10; i < NUM_THREADS; i++, j++) {
    pthread_create(&threads[i], NULL, sqrtWorker, (void *)&j);
  }
  for(i = 0; i < NUM_THREADS; i++) {
    void* status;
    pthread_join(threads[i], &status);
  }
   pthread_exit(NULL);
}
