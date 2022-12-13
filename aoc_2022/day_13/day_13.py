import itertools as it
import math
from ast import literal_eval
from functools import cmp_to_key
from typing import Generator, List, Union

NestedList = List[Union[int, "NestedList"]]


def input_reader(f: str) -> List[NestedList]:
    with open(f, "r", encoding="utf-8") as f:
        packet_series = list(map(literal_eval, [line for line in f if line.strip()]))
    return packet_series


def sign_of_difference(left: int, right: int) -> int:
    return (left - right) // abs(left - right) if left != right else 0


def compare_elements(
    left_packet: NestedList, right_packet: NestedList
) -> Generator[int, None, None]:
    for left, right in it.zip_longest(left_packet, right_packet):
        if isinstance(left, int) and isinstance(right, int):
            yield sign_of_difference(left, right)
        else:
            left = [left] if isinstance(left, int) else left
            right = [right] if isinstance(right, int) else right
            if left is None or right is None:
                yield sign_of_difference(
                    len(left) if left else 0, len(right) if right else 0
                )
            else:
                yield from compare_elements(left, right)


def compare_two_packets(left_packet: NestedList, right_packet: NestedList) -> int:
    is_equal = 0
    for result in compare_elements(left_packet, right_packet):
        if result:
            return result
    else:
        return 0


def packet_checker(packet_series: List[NestedList]) -> List[int]:
    ordered_pairs_idx = [
        i
        for i, packets in enumerate(zip(packet_series[::2], packet_series[1::2]), 1)
        if compare_two_packets(*packets) <= 0
    ]
    return ordered_pairs_idx


def order_packets(
    packet_series: NestedList, dividers: List[NestedList]
) -> List[NestedList]:
    return sorted(packet_series + dividers, key=cmp_to_key(compare_two_packets))


def main():
    DATA = input_reader("./input.txt")
    print(sum(packet_checker(DATA)))  # 4821

    DIVIDERS = [[[2]], [[6]]]
    ordered_packets = order_packets(DATA, DIVIDERS)
    print(
        math.prod(1 + ordered_packets.index(divider) for divider in DIVIDERS)
    )  # 21890


if __name__ == "__main__":
    main()
