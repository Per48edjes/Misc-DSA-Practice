import itertools as it
from typing import Set, Tuple

import numpy as np


def input_reader(f: str) -> Set[Tuple[int, int]]:
    with open(f, "r", encoding="utf-8") as f:
        endpoint_coords = [
            [
                np.array(list(map(int, coord_str.split(","))))
                for coord_str in line.split(" -> ")
            ]
            for line in f.readlines()
        ]
    rock_coords = set()
    for rock in endpoint_coords:
        for start, stop in it.pairwise(rock):
            rock_coords.add(tuple(stop))
            while not all(start == stop):
                rock_coords.add(tuple(start))
                start = start + np.sign(stop - start)
    return rock_coords


def sand_poured(cave_rocks: Set[Tuple[int, int]], q: int):

    sand_entry = (500, 0)
    lowest_rock_y = max(cave_rocks, key=lambda c: c[1])[1]
    blocking = set(cave_rocks)
    match q:
        case 1:
            floor = float("inf")
        case 2:
            floor = 2 + lowest_rock_y

    sand_not_leaking, settled_sand_particles = True, 0
    while sand_not_leaking:

        sand_particle_pos, sand_particle_unsettled = sand_entry, True
        while sand_particle_unsettled:

            sand_x, sand_y = sand_particle_pos

            match q:
                case 1:
                    stopping_cond = sand_y == lowest_rock_y
                case 2:
                    stopping_cond = sand_particle_pos in blocking
            if stopping_cond:
                sand_not_leaking = False
                break

            if (next_y := sand_y + 1) < floor:
                if (down := (sand_x, next_y)) not in blocking:
                    sand_particle_pos = down
                elif (down_left := (sand_x - 1, next_y)) not in blocking:
                    sand_particle_pos = down_left
                elif (down_right := (sand_x + 1, next_y)) not in blocking:
                    sand_particle_pos = down_right
                else:
                    sand_particle_unsettled = False
                    blocking.add(sand_particle_pos)
                    settled_sand_particles += 1
            else:
                sand_particle_unsettled = False
                blocking.add(sand_particle_pos)
                settled_sand_particles += 1

    return settled_sand_particles


def main():
    DATA = input_reader("./input.txt")
    print(sand_poured(DATA, q=1))  # 961
    print(sand_poured(DATA, q=2))  # 26375


if __name__ == "__main__":
    main()
