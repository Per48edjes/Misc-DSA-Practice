from typing import List
import heapq


def input_reader(f: str) -> List[List[int]]:
    data = []
    with open(f, "r", encoding="utf-8") as f:
        lines = f.readlines()
        elf_pack = []
        for line in lines:
            if cals := line.strip():
                elf_pack.append(int(cals))
            else:
                data.append(elf_pack)
                elf_pack = []
    return data


DATA = list(map(sum, input_reader("input.txt")))


def get_top_n_cal_elves(n: int) -> int:
    if n == 1:
        return max(DATA)
    else:
        return sum(heapq.nlargest(n, DATA))


def main():
    print(get_top_n_cal_elves(n=1))  # 71924
    print(get_top_n_cal_elves(n=3))  # 210406


if __name__ == "__main__":
    main()
