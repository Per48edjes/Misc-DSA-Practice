from typing import List, Tuple


def input_reader(f: str) -> List[Tuple[str]]:
    with open(f, "r", encoding="utf-8") as f:
        lines = [line.split() for line in f.readlines()]
    return lines


def shape_indexer(shapes: Tuple[str]) -> Tuple[int]:
    return tuple(
        ord(shape) - (ord("A") if shape in ("A", "B", "C") else ord("X"))
        for shape in shapes
    )


DATA = list(map(shape_indexer, input_reader("./input.txt")))


def outcome_points(shapes: Tuple[int], q: int) -> int:
    opps, you = shapes
    if q == 1:
        return ((you - opps + 1) % 3) * 3
    elif q == 2:
        return you * 3
    else:
        raise ValueError


def shape_points(shapes: Tuple[int], q: int) -> int:
    opps, you = shapes
    if q == 1:
        return you + 1
    elif q == 2:
        return ((you + opps - 1) % 3) + 1
    else:
        raise ValueError


def total_points(q: int = 1) -> int:
    return sum(
        map(lambda shapes: outcome_points(shapes, q) + shape_points(shapes, q), DATA)
    )


def main():
    print(total_points(1))  # 15691
    print(total_points(2))  # 12989


if __name__ == "__main__":
    main()
