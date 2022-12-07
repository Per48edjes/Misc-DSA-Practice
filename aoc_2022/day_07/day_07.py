from __future__ import annotations

from operator import ge, le
from typing import Callable, Generator, List, Optional
from functools import cached_property


class FileTree:
    def __init__(
        self,
        name: str,
        size: Optional[int],
        parent: Optional[FileTree],
        children: Optional[List[FileTree]],
    ):
        self.name = name
        self._size = size
        self.parent = parent
        self.children = children

    @cached_property
    def is_file(self) -> bool:
        return self.children is None

    @property
    def size(self) -> int:
        if self._size is None:
            self._size = sum([child.size for child in self.children])
        return self._size

    def add_child(self, child: FileTree) -> None:
        self.children.append(child)

    def __str__(self) -> str:
        indent, parent = "", self.parent
        while parent:
            indent += "  "
            parent = parent.parent
        stub = " ".join([indent, "-", str((self.name, self.size))])
        if self.is_file:
            return stub
        for child in self.children:
            stub += "\n" + str(child)
        return stub


def input_reader(f: str) -> FileTree:
    with open(f, "r", encoding="utf-8") as f:
        lines = [line.rstrip() for line in f.readlines()]

    root = cur_dir = FileTree(lines.pop(0).split()[-1], None, None, [])
    for line in lines:
        line_parts = line.split()
        if line_parts[0] == "$":
            if line_parts[1] == "cd":
                if line_parts[2] == "..":
                    cur_dir = cur_dir.parent
                else:
                    child_dir = FileTree(line_parts[2], None, cur_dir, [])
                    cur_dir.add_child(child_dir)
                    cur_dir = child_dir
        elif line_parts[0].isnumeric():
            child_file = FileTree(line_parts[1], int(line_parts[0]), cur_dir, None)
            cur_dir.add_child(child_file)

    return root


DATA = input_reader("./input.txt")


def dfs_dir_size_limit(
    filetree: FileTree, threshold: int, comparison: Callable
) -> Generator[FileTree, None, None]:
    if not filetree.is_file:
        if comparison(filetree.size, threshold):
            yield filetree
        for child in filetree.children:
            yield from dfs_dir_size_limit(child, threshold, comparison)


def main():
    max_size = 100_000
    directories = dfs_dir_size_limit(DATA, max_size, le)
    print(sum(directory.size for directory in directories))  # 2104783

    total_disk_capacity = 70_000_000
    required_space = 30_000_000
    directories = dfs_dir_size_limit(
        DATA, required_space - (total_disk_capacity - DATA.size), ge
    )
    print(min(directory.size for directory in directories))  # 5883165


if __name__ == "__main__":
    main()
