import time

# from threading import Thread
from concurrent.futures import ThreadPoolExecutor
from threading import Lock

lock = Lock()


def increment(shared: list[int], pos: int):
    try:
        with lock:
            current = shared[pos]
            time.sleep(0.0001)
            shared[pos] = current + 1
    except IndexError:
        print("Index out of bounds")
        raise


def main():
    shared = [0]

    # threads = []
    # for _ in range(10_000):
    #     t = Thread(target=increment, args=(shared, 0))
    #     threads.append(t)
    #     t.start()
    #
    # for t in threads:
    #     t.join()

    with ThreadPoolExecutor(max_workers=100) as executor:
        futures = [executor.submit(increment, shared, 0) for _ in range(10_000)]
        for future in futures:
            future.result()

    print(shared[0])


if __name__ == "__main__":
    main()
