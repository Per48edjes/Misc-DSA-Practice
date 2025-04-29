import threading
import time

shared = [0]


def worker():
    for _ in range(1000):
        temp = shared[0]
        time.sleep(0.00001)  # force a context switch!
        shared[0] = temp + 1


threads = [threading.Thread(target=worker) for _ in range(10)]
for t in threads:
    t.start()
for t in threads:
    t.join()

print(shared[0])
