import re
from typing import Dict, FrozenSet

from cachetools.func import lru_cache


def input_reader(f: str):
    info = re.compile(
        r"^.*\b([A-Z]{2})\b.*=(\d+);(?:.*valves?\s)((?:(?:[A-Z]{2})(?:,\s)?)+)",
    )
    with open(f, "r", encoding="utf-8") as f:
        graph = {}
        for line in f.readlines():
            m = re.match(info, line.rstrip())
            origin, pressure_release, destinations = (
                m.group(1),
                m.group(2),
                m.group(3).split(", "),
            )
            graph.update(
                {
                    origin: {
                        "pressure_release": int(pressure_release),
                        "neighbors": destinations,
                    }
                }
            )
    return graph


def total_pressure_released(
    graph: Dict[str, Dict[str, int]], time: int, start: str, players: int = 1
) -> int:
    @lru_cache(maxsize=None)
    def dfs_traverse_valves(
        current_valve: str, mins_left: int, opened_valves: FrozenSet[str], players: int
    ) -> int:

        if mins_left == 0:
            return (
                0
                if players == 1
                else dfs_traverse_valves(start, time, opened_valves, players - 1)
            )

        max_pressure_release = 0

        if (
            current_valve_release_rate := graph[current_valve]["pressure_release"]
        ) > 0 and current_valve not in opened_valves:
            current_valve_release = (
                current_valve_release_rate * (mins_left - 1)
            ) + dfs_traverse_valves(
                current_valve, mins_left - 1, opened_valves | {current_valve}, players
            )
            max_pressure_release = max(max_pressure_release, current_valve_release)

        for neighbor in graph[current_valve]["neighbors"]:
            next_valve_release = dfs_traverse_valves(
                neighbor, mins_left - 1, opened_valves, players
            )
            max_pressure_release = max(max_pressure_release, next_valve_release)

        return max_pressure_release

    return dfs_traverse_valves(start, time, frozenset(), players)


def main():
    DATA = input_reader("./input.txt")
    print(total_pressure_released(DATA, 30, "AA"))  # 1653
    print(total_pressure_released(DATA, 26, "AA", 2))  # 2223


if __name__ == "__main__":
    main()
