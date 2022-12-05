import re
from collections import defaultdict, deque
from typing import DefaultDict, Deque, List, Tuple


def input_reader(f: str) -> Tuple[List[List[int]], DefaultDict[int, Deque]]:
    with open(f, "r", encoding="utf-8") as f:
        lines = [line.rstrip() for line in f.readlines()]
        directions, stacks = [], defaultdict(deque)
        for line in lines:
            if line.startswith("move"):
                directions.append([int(n) for n in re.findall(r"\d+", line)])
            else:
                for match in re.finditer(r"\b[A-Z]\b", line):
                    stacks[match.start() // 4 + 1].append(match.group())
        return directions, stacks


DATA = input_reader("./input.txt")
DATA_2 = input_reader("./input.txt")


def move_crates(
    directions: List[List[int]], stacks: DefaultDict[int, Deque], q: int
) -> None:
    assert q in (1, 2), "Invalid question argument!"
    for num_crates, origin, destination in directions:
        crates = [stacks[origin].popleft() for _ in range(num_crates)]
        stacks[destination].extendleft(crates if q == 1 else reversed(crates))


def top_crates(stacks: DefaultDict[int, Deque]) -> str:
    top_crates = [stack[0] for _, stack in sorted(stacks.items()) if stack]
    return "".join(top_crates)


def main():
    directions, stacks = DATA
    move_crates(directions, stacks, 1)
    print(top_crates(stacks))  # HNSNMTLHQ

    directions, stacks = DATA_2
    move_crates(directions, stacks, 2)
    print(top_crates(stacks))  # RNLFDJMCT


if __name__ == "__main__":
    main()
