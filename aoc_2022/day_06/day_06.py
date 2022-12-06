def input_reader(f: str) -> str:
    with open(f, "r", encoding="utf-8") as f:
        lines = f.readlines()
    return lines[0]


DATA = input_reader("./input.txt")


def first_marker(data_stream: str, buffer_size: int = 4) -> int:
    beg_idx = end_idx = 0
    buffer = set()
    while len(buffer) < buffer_size:
        buffer.add(data_stream[end_idx])
        end_idx += 1
        if len(buffer) < (end_idx - beg_idx):
            beg_idx += 1
            end_idx = beg_idx
            buffer.clear()
    return end_idx


def main():
    print(first_marker(DATA, 4))  # 1707
    print(first_marker(DATA, 14))  # 3697


if __name__ == "__main__":
    main()
