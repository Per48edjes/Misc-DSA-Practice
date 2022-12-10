from itertools import islice
from typing import Generator, Iterable, List, Tuple, Union


def input_reader(f: str) -> List[Union[Tuple[str, int], Tuple[str]]]:
    with open(f, "r", encoding="utf-8") as f:
        lines = [
            tuple(
                map(
                    lambda s: int(s) if s.lstrip("-").isnumeric() else s,
                    line.rstrip().split(),
                )
            )
            for line in f.readlines()
        ]
    return lines


DATA = input_reader("./input.txt")


def batched(iterable: Iterable, n: int) -> Generator:
    if n < 1:
        raise ValueError("n must be at least one")
    itr = iter(iterable)
    while batch := list(islice(itr, n)):
        yield batch


def x_reg_cycle_values(
    instructions: List[Union[Tuple[str, int], Tuple[str]]]
) -> Generator[int, None, None]:

    x_reg = 1

    for instruction in instructions:
        match instruction[0]:
            case "addx":
                cycles, V = 2, instruction[1]
            case "noop":
                cycles, V = 1, None
            case _:
                raise ValueError
        for cycle_i in range(cycles):
            yield x_reg
            x_reg += V if cycle_i and V else 0


def signal_strength(
    instructions: List[Union[Tuple[str, int], Tuple[str]]],
    start_cycle: int,
    end_cycle: int,
    period: int,
) -> int:

    result, cycle_count = 0, 0

    for cycle_value in x_reg_cycle_values(instructions):
        cycle_count += 1
        if (cycle_count - start_cycle) % period == 0:
            result += cycle_value * cycle_count
            if cycle_count == end_cycle:
                break

    return result


def crt_drawing(
    instructions: List[Union[Tuple[str, int], Tuple[str]]],
    crt_width: int,
    crt_height: int,
) -> None:
    crt_display = ["."] * (crt_width * crt_height)
    sprite_idx_offsets = [-1, 0, 1]
    cycle_count = 0

    for cycle_value in x_reg_cycle_values(instructions):
        crt_pos = cycle_count
        cycle_count += 1
        sprite_pixels_pos = [
            crt_width * (crt_pos // crt_width) + cycle_value + offset
            for offset in sprite_idx_offsets
        ]
        if crt_pos in sprite_pixels_pos:
            crt_display[crt_pos] = "#"

    for row in batched(crt_display, crt_width):
        print("".join(row))


def main():
    print(signal_strength(DATA, 20, 220, 40))  # 14040
    crt_drawing(DATA, 40, 6)
    """
    ####..##...##....##.####...##.####.#....
    ...#.#..#.#..#....#....#....#.#....#....
    ..#..#....#.......#...#.....#.###..#....
    .#...#.##.#.......#..#......#.#....#....
    #....#..#.#..#.#..#.#....#..#.#....#....
    ####..###..##...##..####..##..#....####.
    """


if __name__ == "__main__":
    main()
