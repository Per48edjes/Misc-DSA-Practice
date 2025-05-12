from __future__ import annotations

from typing import Any, Generator

from binarytree import Node, build


def inorder_traversal(
    node: Node | None, revalue: bool = False
) -> Generator[Any | None, int, None]:
    """
    Return a generator that yields inorder traversal of a binary tree rooted at this node
    """
    if node:
        if node.left:
            yield from inorder_traversal(node.left, revalue)
        new_value = yield node.value
        if revalue and new_value is not None:
            node.value = new_value
        if node.right:
            yield from inorder_traversal(node.right, revalue)


def relabel_tree(root: Node | None, new_values: list[Any]):
    gen = inorder_traversal(root, revalue=True)
    next(gen)  # Primes the generator, but lose the first value :(
    for new_value in new_values:
        try:
            gen.send(new_value)  # Send the new value to replace it
        except StopIteration:
            break


if __name__ == "__main__":
    values = [1, 2, 3, 4, 5, 6, 7]
    new_values = ["a", "b", "c", "d", "e", "f", "g"]

    root = build(values)
    print(root)
    print(list(inorder_traversal(root)))

    relabel_tree(root, new_values)
    print(root)
    print(list(inorder_traversal(root)))
