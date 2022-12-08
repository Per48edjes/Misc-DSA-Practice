import itertools as it
from functools import reduce
from operator import mul, or_
from typing import Callable, List

import numpy as np


def input_reader(f: str) -> List[List[int]]:
    with open(f, "r", encoding="utf-8") as f:
        lines = [list(map(int, line.rstrip())) for line in f.readlines()]
    return lines


DATA = input_reader("./input.txt")


def tree_viz_1d(trees: np.array, ascending: bool = True) -> np.array:
    start, stop, step, max_height_idx = (
        1 if ascending else len(trees) - 2,
        len(trees) - 1 if ascending else 0,
        1 if ascending else -1,
        0 if ascending else -1,
    )
    max_height = trees[max_height_idx]
    viz_mask = [True] * len(trees)

    for idx in range(start, stop, step):
        if is_viz := trees[idx] > max_height:
            max_height = trees[idx]
        viz_mask[idx] = is_viz

    return np.array(viz_mask)


def tree_score_1d(trees: np.array, ascending: bool = True) -> np.array:
    start, stop, step = (
        1 if ascending else len(trees) - 2,
        len(trees) if ascending else -1,
        1 if ascending else -1,
    )
    scenic_scores = [0] * len(trees)

    for idx in range(start, stop, step):
        neighbor_heights = trees[:idx][::-1] if ascending else trees[idx + 1:]
        for distance, neighbor_height in enumerate(neighbor_heights, 1):
            if trees[idx] <= neighbor_height:
                scenic_scores[idx] += distance
                break
        else:
            scenic_scores[idx] += distance

    return np.array(scenic_scores)


def tree_2d_grids(
    trees: List[List[int]], eval_1d: Callable, operation: Callable
) -> np.array:
    trees = np.array(trees)
    return reduce(
        operation,
        [
            np.apply_along_axis(eval_1d, axis, trees, ascending=is_ascending)
            for axis, is_ascending in it.product([0, 1], [True, False])
        ],
    )


def main():
    visibility_grid = tree_2d_grids(DATA, tree_viz_1d, or_)
    scenic_score_grid = tree_2d_grids(DATA, tree_score_1d, mul)

    print(np.count_nonzero(visibility_grid))  # 1805
    print(np.max(scenic_score_grid))  # 444528


if __name__ == "__main__":
    main()
