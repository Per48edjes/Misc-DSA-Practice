import time
from threading import Thread


def increment(shared: list[int], pos: int):
    try:
        current = shared[pos]
        time.sleep(0.001)
        shared[pos] = current + 1
    except IndexError:
        print("Index out of bounds")
        raise


def main():
    shared = [0]

    threads = []
    for _ in range(10000):
        t = Thread(target=increment, args=(shared, 0))
        threads.append(t)
        t.start()

    for t in threads:
        t.join()

    print(shared[0])


if __name__ == "__main__":
    main()
