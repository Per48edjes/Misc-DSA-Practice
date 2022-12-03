from typing import Callable, List, Union


def input_reader(f: str) -> List[str]:
    with open(f, "r", encoding="utf-8") as f:
        rucksacks = [line.rstrip() for line in f.readlines()]
    return rucksacks


DATA = input_reader("./input.txt")


def get_priority(letter: str) -> int:
    if letter.islower():
        return ord(letter) - ord("a") + 1
    else:
        return ord(letter) - ord("A") + 26 + 1


def group_by(lst: List, n: int = 1) -> Union[List, List[List]]:
    if n == 1:
        return lst
    return [lst[i : i + n] for i in range(0, len(lst), n)]


def priority_total(group_size: int = 1) -> Callable:
    def decorator(func: Callable) -> Callable:
        def wrapper(data: List[str]) -> int:
            return sum(func(*group) for group in group_by(data, group_size))

        return wrapper

    return decorator


@priority_total(1)
def dupe_individual_compartments(*rucksacks: str) -> int:
    compt_1, compt_2 = set(rucksacks[: len(rucksacks) // 2]), set(
        rucksacks[len(rucksacks) // 2 :]
    )
    return get_priority(compt_1.intersection(compt_2).pop())


@priority_total(3)
def dupe_group_rucksacks(*rucksacks: str) -> int:
    first_rucksack, *rest_rucksacks = [set(rucksack) for rucksack in rucksacks]
    return get_priority(first_rucksack.intersection(*rest_rucksacks).pop())


def main():
    print(dupe_individual_compartments(DATA))  # 8233
    print(dupe_group_rucksacks(DATA))  # 2821


if __name__ == "__main__":
    main()
