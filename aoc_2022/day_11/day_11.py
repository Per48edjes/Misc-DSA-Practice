from __future__ import annotations

import re
from collections import deque
from dataclasses import dataclass
from itertools import islice
from math import lcm, prod
from operator import add, floordiv, mod, mul
from typing import Any, Callable, ClassVar, List, Tuple, Union


def input_reader(f: str) -> List[List[Any]]:
    line_chunk_size = 7
    number_pattern = re.compile(r"\b(\d+)\b")
    operation_pattern = re.compile(r"([\+\-\*\/])\s(\d+|old)")
    with open(f, "r", encoding="utf-8") as f:
        for notes in iter(
            lambda: tuple(map(str.strip, islice(f, line_chunk_size))), ()
        ):
            monkey_params = []
            for line in notes[: line_chunk_size - 1]:
                number_matches = number_pattern.findall(line)
                if line.startswith("Starting items"):
                    extracted_value = deque(map(int, number_matches))
                elif line.startswith("Operation"):
                    operator, operand = operation_pattern.search(line).groups()
                    match operator:
                        case "+":
                            fn = add
                        case "*":
                            fn = mul
                    extracted_value = (
                        operand if operand == "old" else int(operand),
                        fn,
                    )
                else:
                    extracted_value = int(number_matches[0])
                monkey_params.append(extracted_value)
            Monkey(*monkey_params)


@dataclass
class Monkey:
    monkeys: ClassVar[List[Monkey]] = []
    idx: int
    items: deque
    _operation: Tuple[Callable, Union[str, int]]
    test_divisor: int
    true_monkey_idx: int
    false_monkey_idx: int
    monkey_business: int = 0

    def __post_init__(self):
        self.monkeys.append(self)
        self.operand, self.fn = self._operation

    def _inspect_item(self, item: int, q: int):
        self.monkey_business += 1
        operand = item if self.operand == "old" else self.operand
        worry_level = self.fn(item, operand)

        match q:
            case 1:
                worry_level = floordiv(worry_level, 3)
            case 2:
                worry_level = mod(
                    worry_level, lcm(*[m.test_divisor for m in self.monkeys])
                )
            case _:
                raise ValueError

        if mod(worry_level, self.test_divisor) == 0:
            self.monkeys[self.true_monkey_idx].items.append(worry_level)
        else:
            self.monkeys[self.false_monkey_idx].items.append(worry_level)

    def complete_turn(self, q: int):
        while self.items:
            self._inspect_item(self.items.popleft(), q)


def monkey_around(rounds: int, q: int) -> int:
    for round_num in range(rounds):
        for monkey in Monkey.monkeys:
            monkey.complete_turn(q)


def monkey_businesses(top_n: int) -> int:
    return prod(
        sorted([monkey.monkey_business for monkey in Monkey.monkeys], reverse=True)[
            :top_n
        ]
    )


def main():
    input_reader("./input.txt")
    monkey_around(20, q=1)
    print(monkey_businesses(2))  # 101436

    Monkey.monkeys.clear()

    input_reader("./input.txt")
    monkey_around(10_000, q=2)
    print(monkey_businesses(2))  # 19754471646


if __name__ == "__main__":
    main()
