from __future__ import annotations

import heapq
import math
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple, Union


def input_reader(f: str) -> Graph:

    with open(f, "r", encoding="utf-8") as f:
        lines = []
        for i, row in enumerate(f.readlines()):
            line = []
            for j, letter in enumerate(row.rstrip()):
                if letter == "S":
                    start, letter = (i, j), "a"
                if letter == "E":
                    end, letter = (i, j), "z"
                value = ord(letter) - ord("a")
                line.append(value)
            lines.append(line)

    return Graph(lines, start, end)


@dataclass
class Graph:
    _graph: List[List[int]]
    start: Tuple[int, int]
    end: Tuple[int, int]
    distance_to: Dict[Tuple[int, int], int] = field(default_factory=dict)

    def __post_init__(self):
        self.distance_to[self.start] = 0
        self._rows = len(self._graph)
        self._cols = len(self._graph[0])

    def _get_neighbors(self, current: Tuple[int, int]) -> List[Tuple[int, int]]:

        cur_i, cur_j = current

        neighbors = filter(
            lambda t: t[0] >= 0
            and t[1] >= 0
            and t[0] < self._rows
            and t[1] < self._cols
            and self._graph[cur_i][cur_j] + 1 >= self._graph[t[0]][t[1]],
            [
                (cur_i + 1, cur_j),
                (cur_i - 1, cur_j),
                (cur_i, cur_j + 1),
                (cur_i, cur_j - 1),
            ],
        )
        return list(neighbors)

    def _get_distance(
        self, origin: Tuple[int, int], destination: Tuple[int, int]
    ) -> int:

        return 1

    def _heuristic(self, current: Tuple[int, int]) -> int:

        cur_i, cur_j = current
        end_i, end_j = self.end

        l1_norm = abs(cur_i - end_i) + abs(cur_j - end_j)

        return l1_norm

    def a_star_shortest_path(
        self, start: Optional[Tuple[int, int]] = None, multiple_traversals: bool = True
    ) -> Union[int, math.inf]:

        start = self.start if start is None else start
        frontier_nodes = [(self.distance_to.setdefault(start, 0), start)]

        while frontier_nodes:
            _, current = heapq.heappop(frontier_nodes)
            if current == self.end:
                break
            for neighbor in self._get_neighbors(current):
                new_cost = self.distance_to[current] + self._get_distance(
                    current, neighbor
                )
                if (
                    neighbor not in self.distance_to
                    or new_cost < self.distance_to[neighbor]
                ):
                    self.distance_to[neighbor] = new_cost
                    priority = new_cost + self._heuristic(neighbor)
                    heapq.heappush(frontier_nodes, (priority, neighbor))

        shortest_path_len = self.distance_to.get(self.end, math.inf)

        if multiple_traversals:
            self.distance_to.clear()

        return shortest_path_len

    def shortest_path_optimal_start(self) -> int:

        starts = []

        for i, row in enumerate(self._graph):
            for j, _ in enumerate(row):
                is_a = self._graph[i][j] == 0
                next_to_b = any(
                    self._graph[m][n] == 1 for m, n in self._get_neighbors((i, j))
                )
                if is_a and next_to_b:
                    starts.append((i, j))

        shortest_path_len = min(
            map(lambda start: self.a_star_shortest_path(start, True), starts)
        )

        return shortest_path_len


def main():
    DATA = input_reader("./input.txt")
    print(DATA.a_star_shortest_path(multiple_traversals=True))  # 517
    print(DATA.shortest_path_optimal_start())  # 512


if __name__ == "__main__":
    main()
