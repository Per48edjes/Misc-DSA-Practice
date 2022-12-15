import re
from typing import List, Optional, Set, Tuple, Union


def input_reader(f: str) -> List[List[Tuple[int, int]]]:
    with open(f, "r", encoding="utf-8") as f:
        sensors = []
        for line in f.readlines():
            coords = list(map(int, re.findall(r"=(-?\d+)", line)))
            sensors.append(list(zip(coords[::2], coords[1::2])))
    return sensors


def manhattan_distance(p1: Tuple[int, int], p2: Tuple[int, int]) -> int:
    p1_x, p1_y = p1
    p2_x, p2_y = p2
    return abs(p1_x - p2_x) + abs(p1_y - p2_y)


def interval_covering(
    start: int, stop: int, sorted_intervals: List[Tuple[int, int]], max_dim: int
) -> int:
    for covering_interval in sorted_intervals:
        if stop >= max_dim:
            return max_dim - (stop + 1)
        if stop + 1 < covering_interval[0]:
            return stop + 1
        if (interval_right := covering_interval[1]) > stop:
            stop = interval_right
    return -1


def row_inspector(
    sensors: List[List[Tuple[int, int]]], row: int, max_dim: Optional[int] = None
) -> Union[int, Set[int]]:
    def sensor_coverage(
        sensor_coord: Tuple[int, int], beacon_coord: Tuple[int, int]
    ) -> Set[Tuple[int, int]]:
        max_dist = manhattan_distance(sensor_coord, beacon_coord)
        sensor_coord_x, sensor_coord_y = sensor_coord
        beacon_coord_x, beacon_coord_y = beacon_coord
        surrounding_coords = set()
        if abs(row - sensor_coord_y) <= max_dist:
            x_dof = max_dist - abs(row - sensor_coord_y)
            if max_dim is None:
                for dx in range(-x_dof, x_dof + 1):
                    surrounding_coords.add((sensor_coord_x + dx))
            else:
                surrounding_coords.add((sensor_coord_x - x_dof, sensor_coord_x + x_dof))
        return surrounding_coords

    total_coverage, known_beacon_locs = set(), set()
    for sensor_coord, beacon_coord in sensors:
        beacon_coord_x, beacon_coord_y = beacon_coord
        total_coverage |= sensor_coverage(sensor_coord, beacon_coord)
        if beacon_coord_y == row:
            known_beacon_locs.add(beacon_coord_x)

    if max_dim is None:
        return len(total_coverage - known_beacon_locs)
    else:
        return interval_covering(
            -1, -1, sorted(total_coverage, key=lambda t: (t[0], -t[1])), max_dim
        )


def distress_beacon_freq(sensors: List[List[Tuple[int, int]]], max_dim: int) -> int:
    for row in range(max_dim + 1):
        if (distress_beacon_x := row_inspector(sensors, row, max_dim)) >= 0:
            return 4_000_000 * distress_beacon_x + row


def main():
    DATA = input_reader("./input.txt")
    print(row_inspector(DATA, 2_000_000))  # 6275922
    print(distress_beacon_freq(DATA, 4_000_000))  #  11747175442119


if __name__ == "__main__":
    main()
