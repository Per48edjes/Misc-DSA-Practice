from typing import Any, Generator

from binarytree import Node, build


def inorder_traversal(node: Node | None) -> Generator[Any | None, Any, None]:
    """
    Return a generator that yields inorder traversal of a binary tree rooted at this node
    """
    if node:
        if node.left:
            yield from inorder_traversal(node.left)
        if new_value := (yield node.value):
            node.value = new_value
        if node.right:
            yield from inorder_traversal(node.right)


def relabel_tree(root: Node | None, new_values: list[Any]):
    """
    Relabel the tree with the given new values
    """
    gen = inorder_traversal(root)
    next(gen)  # Primes the generator, but lose the first value
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
