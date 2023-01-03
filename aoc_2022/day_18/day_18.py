import itertools as it
from collections import Counter, deque
from typing import Generator

import numpy as np


def input_reader(f: str) -> list[np.array]:
    with open(f, "r", encoding="utf-8") as f:
        coordinates = [
            np.array(list(map(int, line.rstrip().split(",")))) for line in f.readlines()
        ]
    return coordinates


def check_matching_coords_in_dims(
    pair: tuple[np.array], match_dims: tuple[int]
) -> bool:
    first, second = pair
    comp_arr = first - second == np.zeros_like(first)
    return all(comp_arr[dim] for dim in match_dims)


def get_neighbors(coordinate: tuple[int]) -> Generator[tuple[int], None, None]:
    x, y, z = coordinate
    for dx, dy, dz in filter(
        lambda t: Counter(t)[0] == 2,
        [offset for offset in it.product((-1, 0, 1), repeat=3)],
    ):
        yield (x + dx, y + dy, z + dz)


def surface_area(cube_coords: list[np.array]) -> int:
    hidden_faces, dims = set(), deque(range(3))
    for _ in range(3):
        for pair in it.pairwise(
            sorted(cube_coords, key=lambda a: tuple(a[i] for i in dims))
        ):
            match_dims = tuple(dims)[:2]
            if check_matching_coords_in_dims(pair, match_dims):
                first, second = pair
                delta = second - first
                if np.sum(np.absolute(delta)) == 1:
                    hidden_faces.add(frozenset(map(tuple, pair)))
        dims.rotate()
    surface_area = (len(cube_coords) * 6) - (2 * len(hidden_faces))
    return surface_area


def find_hidden_cubes(cube_coords: list[np.array]) -> list[np.array]:
    max_coord, min_coord = (
        np.maximum.reduce(cube_coords) + 1,
        np.minimum.reduce(cube_coords) - 1,
    )
    cuboid, rocks, filled = (
        set(
            tuple(min_coord + np.array(offset))
            for offset in it.product(
                *list(map(lambda x: list(range(x + 1)), max_coord - min_coord))
            )
        ),
        set(tuple(a) for a in cube_coords),
        set(),
    )
    q = deque([tuple(min_coord)])
    while q:
        current_coord = q.popleft()
        if not (current_coord in rocks or current_coord in filled):
            for neighbor in get_neighbors(current_coord):
                if neighbor in cuboid:
                    q.append(neighbor)
            filled.add(current_coord)
    return list(map(np.array, cuboid - (rocks | filled)))


def main():
    DATA = input_reader("./test.txt")
    total_surface_area = surface_area(DATA)
    print(total_surface_area)  # 64
    interior_surface_area = surface_area(find_hidden_cubes(DATA))
    exterior_surface_area = total_surface_area - interior_surface_area
    print(exterior_surface_area)  # 58

    DATA = input_reader("./input.txt")
    total_surface_area = surface_area(DATA)
    print(total_surface_area)  # 4314
    interior_surface_area = surface_area(find_hidden_cubes(DATA))
    exterior_surface_area = total_surface_area - interior_surface_area
    print(exterior_surface_area)  # 2444


if __name__ == "__main__":
    main()
