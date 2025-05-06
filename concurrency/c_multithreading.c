#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define NUM_THREADS 100
#define NUM_TASKS 10000

pthread_mutex_t lock;
int shared = 0;

void *increment(void *arg) {
  pthread_mutex_lock(&lock);
  shared++;
  usleep(100); // Sleep for 100 microseconds
  pthread_mutex_unlock(&lock);

  return NULL;
}

typedef struct {
  pthread_t thread;
  int task_count;
} ThreadPool;

void *worker(void *arg) {
  ThreadPool *pool = (ThreadPool *)arg;

  for (int i = 0; i < pool->task_count; i++) {
    increment(NULL);
  }

  return NULL;
}

int main() {
  pthread_t threads[NUM_THREADS];

  if (pthread_mutex_init(&lock, NULL) != 0) {
    printf("Mutex init failed\n");
    return 1;
  }

  ThreadPool pool[NUM_THREADS];

  for (int i = 0; i < NUM_THREADS; i++) {
    pool[i].task_count = NUM_TASKS / NUM_THREADS;
    pthread_create(&threads[i], NULL, worker, &pool[i]);
  }

  for (int i = 0; i < NUM_THREADS; i++) {
    pthread_join(threads[i], NULL);
  }

  pthread_mutex_destroy(&lock);

  printf("%d\n", shared);
  return 0;
}
