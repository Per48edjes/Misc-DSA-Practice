from operator import attrgetter

import pytest
from binarytree import Node, build

from trees import inorder_traversal, relabel_tree


def test_inorder_traversal_single_element():
    root = Node(1)
    assert list(inorder_traversal(root)) == [1]


def test_inorder_traversal_multiple_elements():
    level_order_labels = [4, 2, 5, 1, 3]
    root = build([1, 2, 3, 4, 5])
    assert list(inorder_traversal(root)) == level_order_labels


def test_inorder_traversal_relabel():
    values = [1, 2, 3, 4, 5, 6, 7]
    new_values = ["a", "b", "c", "d", "e", "f", "g"]
    root = build(values)
    relabel_tree(root, new_values)
    assert list(inorder_traversal(root)) == list(map(attrgetter("value"), root.inorder))


if __name__ == "__main__":
    pytest.main()
