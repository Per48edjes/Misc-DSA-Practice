import itertools as it
from typing import List, Set, Tuple

import numpy as np


def input_reader(f: str) -> List[Tuple[np.array, int]]:
    with open(f, "r", encoding="utf-8") as f:
        lines = [line.rstrip().split() for line in f.readlines()]

    direction_map = {
        "U": np.array([0, 1]),
        "R": np.array([1, 0]),
        "L": np.array([-1, 0]),
        "D": np.array([0, -1]),
    }
    directions = [
        (direction_map[direction], int(magnitude)) for direction, magnitude in lines
    ]

    return directions


DATA = input_reader("./input.txt")


def unique_tail_positions(
    directions: List[Tuple[np.array, int]], knots: int = 2
) -> Set[Tuple[int, int]]:
    knot_pos, tail_visited_pos = [np.array([0, 0]) for _ in range(knots)], set()
    for direction, magnitude in directions:
        for _ in range(magnitude):
            for lead_idx, trail_idx in it.pairwise(range(knots)):
                is_head, is_tail = lead_idx == 0, trail_idx == knots - 1
                if is_head:
                    knot_pos[lead_idx] += direction
                coord_delta = knot_pos[lead_idx] - knot_pos[trail_idx]
                if any(np.absolute(coord_delta) > 1):
                    knot_pos[trail_idx] += (np.absolute(coord_delta) > 0) * np.sign(
                        coord_delta
                    )
                if is_tail:
                    tail_visited_pos.add(tuple(knot_pos[trail_idx]))

    return tail_visited_pos


def main():
    print(len(unique_tail_positions(DATA, 2)))  # 5930
    print(len(unique_tail_positions(DATA, 10)))  # 2443


if __name__ == "__main__":
    main()
