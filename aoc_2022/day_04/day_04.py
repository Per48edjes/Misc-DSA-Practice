from operator import sub, xor
from typing import List, Tuple


def input_reader(f: str) -> List[Tuple[Tuple[int]]]:
    with open(f, "r", encoding="utf-8") as f:
        lines = [
            tuple(
                map(
                    lambda p: tuple(int(k) for k in p.split("-")),
                    line.rstrip().split(","),
                )
            )
            for line in f.readlines()
        ]
    return lines


DATA = input_reader("./input.txt")


def fully_contained_interval(intervals: Tuple[Tuple[int]]) -> bool:
    lower_diff, upper_diff = [sub(*bounds) for bounds in zip(*intervals)]
    return lower_diff == 0 or upper_diff == 0 or xor(lower_diff > 0, upper_diff > 0)


def overlap_interval(intervals: Tuple[Tuple[int]]) -> bool:
    first, second = intervals
    return not (max(first) < min(second) or max(second) < min(first))


def main():
    print(sum(map(fully_contained_interval, DATA)))  # 453
    print(sum(map(overlap_interval, DATA)))  # 919


if __name__ == "__main__":
    main()
