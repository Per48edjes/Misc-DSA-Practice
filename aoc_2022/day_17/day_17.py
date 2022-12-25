from __future__ import annotations

from collections import namedtuple
from dataclasses import dataclass, field
from typing import List, Optional, Set, Tuple


@dataclass
class Tunnel:
    jet_pattern_input: str
    total_rocks: int
    right_edge: int = 7
    left_edge: int = -1
    bottom_edge: int = 0
    start_row_offset: int = 3 + 1
    start_col_offset: int = 2
    occupied_positions: Set[Tuple[int, int]] = field(default_factory=set)
    column_peaks: List[int] = field(default_factory=list)

    def __post_init__(self):
        self.column_peaks = self.right_edge * [0]
        with open(self.jet_pattern_input, "r", encoding="utf-8") as f:
            self.jet_pattern = [
                (0, -1) if c == "<" else (0, 1) for c in f.read().rstrip()
            ]

    @property
    def simulated_highest_point(self) -> int:
        return (
            max(self.occupied_positions, key=lambda t: t[0])[0]
            if self.occupied_positions
            else 0
        )

    @property
    def simulated_relative_column_peaks(self) -> Tuple[int, ...]:
        relative_column_peaks = []
        for col_peak in self.column_peaks:
            relative_column_peaks.append(self.simulated_highest_point - col_peak)
        return tuple(relative_column_peaks)

    @property
    def highest_point(self) -> int:
        RockState = namedtuple(
            "RockState", "left_edge jet_idx rock_shape_idx relative_column_peaks"
        )
        RockProfile = namedtuple("RockProfile", "rock_idx highest_point_when_settled")
        jet_idx, cycle_detected = 0, False
        settled_rock_states = {}

        def simulate_falling_rocks(
            total_rocks: int, rock_idx: int = 0
        ) -> Optional[Tuple[int, ...]]:
            nonlocal jet_idx
            for _ in range(total_rocks):
                rock, move, old_highest_point = (
                    Rock(rock_idx, self),
                    0,
                    self.simulated_highest_point,
                )
                while not rock.is_settled:
                    direction = (-1, 0) if move % 2 == 1 else self.jet_pattern[jet_idx]
                    jet_idx += 0 if move % 2 == 1 else 1
                    jet_idx %= len(self.jet_pattern)
                    rock.update_shape(*direction)
                    move += 1
                self.occupied_positions |= set(rock.shape)
                self.update_column_peaks(rock)
                if not cycle_detected:
                    if cycle_info := check_for_cycle(rock):
                        return cycle_info
                rock_idx += 1

        def check_for_cycle(rock: Rock) -> Optional[Tuple[int, int]]:
            nonlocal cycle_detected
            repeat_rock_state = RockState(
                rock.left_edge,
                jet_idx,
                rock._rock_idx % 5,
                self.simulated_relative_column_peaks,
            )
            last_repeat_rock = settled_rock_states.setdefault(
                repeat_rock_state,
                RockProfile(rock._rock_idx, self.simulated_highest_point),
            )
            if cycle_detected := (rock._rock_idx > last_repeat_rock.rock_idx):
                cycle_height = (
                    self.simulated_highest_point
                    - last_repeat_rock.highest_point_when_settled
                )
                cycles, remaining_rocks = divmod(
                    self.total_rocks - rock._rock_idx - 1,
                    rock._rock_idx - last_repeat_rock.rock_idx,
                )
                return (
                    cycles * cycle_height,
                    self.simulated_highest_point,
                    remaining_rocks,
                    repeat_rock_state.relative_column_peaks,
                    rock._rock_idx + 1,
                )

        def finish_simulation(
            remaining_rocks: int,
            relative_column_peaks: Tuple[int, ...],
            next_rock_idx: int,
        ) -> int:
            base_offset = max(relative_column_peaks)
            self.occupied_positions = set(
                (abs(col_peak - base_offset), col)
                for col, col_peak in enumerate(relative_column_peaks)
            )
            simulate_falling_rocks(remaining_rocks, next_rock_idx)
            return max(0, self.simulated_highest_point - base_offset)

        cycle_info = simulate_falling_rocks(self.total_rocks)
        if cycle_info:
            cycled_height, beginning_height, *remaining_simulation_args = cycle_info
            remaining_height = finish_simulation(*remaining_simulation_args)
            return beginning_height + cycled_height + remaining_height
        else:
            return self.simulated_highest_point

    def update_column_peaks(self, rock: Rock):
        for row, col in rock.shape:
            self.column_peaks[col] = max(row, self.column_peaks[col])


class Rock:
    def __init__(self, rock_idx: int, tunnel: Tunnel):
        self._rock_idx = rock_idx
        self.shape = self._get_rock_shape(rock_idx)
        self.is_settled = False
        self.tunnel = tunnel
        self.update_shape(
            self.tunnel.start_row_offset + self.tunnel.simulated_highest_point,
            self.tunnel.start_col_offset,
        )

    @property
    def left_edge(self) -> int:
        return min(self.shape, key=lambda t: t[1])[1]

    @property
    def right_edge(self) -> int:
        return max(self.shape, key=lambda t: t[1])[1]

    @property
    def bottom_edge(self) -> int:
        return min(self.shape, key=lambda t: t[0])[0]

    @property
    def top_edge(self) -> int:
        return max(self.shape, key=lambda t: t[0])[0]

    def update_shape(self, row_shift: int, col_shift: int):
        old_shape = list(self.shape)
        self.shape = [(row + row_shift, col + col_shift) for row, col in self.shape]
        if not self._valid_shape():
            self.shape = old_shape
            if row_shift == -1 and col_shift == 0:
                self.is_settled = True

    def _valid_shape(self) -> bool:
        if (
            self.left_edge <= self.tunnel.left_edge
            or self.right_edge >= self.tunnel.right_edge
            or self.bottom_edge <= self.tunnel.bottom_edge
        ):
            return False
        for coord in self.shape:
            if coord in self.tunnel.occupied_positions:
                return False
        return True

    def _get_rock_shape(self, rock_idx: int) -> List[Tuple[int, int]]:
        match rock_idx % 5:
            case 0:
                return [(0, col) for col in range(4)]
            case 1:
                return [
                    (row, col)
                    for col in range(3)
                    for row in range(3)
                    if row == 1 or col == 1
                ]
            case 2:
                return [
                    (row, col)
                    for col in range(3)
                    for row in range(3)
                    if row == 0 or col == 2
                ]
            case 3:
                return [(row, 0) for row in range(4)]
            case 4:
                return [(row, col) for col in range(2) for row in range(2)]


def main():
    t = Tunnel("./test.txt", 2022)
    print(t.highest_point)  # 3068

    t = Tunnel("./test.txt", 1_000_000_000_000)
    print(t.highest_point)  # 1514285714288

    t = Tunnel("./input.txt", 2022)
    print(t.highest_point)  # 3151

    t = Tunnel("./input.txt", 1_000_000_000_000)
    print(t.highest_point)  # 1560919540245


if __name__ == "__main__":
    main()
