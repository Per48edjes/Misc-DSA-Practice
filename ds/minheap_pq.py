from typing import Optional


class MinHeap:

    """
    A MinHeap is a complete binary tree where the value of each node is less than or equal
    to the value of its children. (The root node is the minimum element in the heap.)

    This implementation supports the following operations:
        - peek: Returns the minimum element without removing it from the heap
        - insert: Inserts the item into the heap and maintains the heap invariant property
        - delete: Removes the minimum element from the heap, returns it, and maintains the
                  heap invariant property
    """

    def __init__(self, arr: Optional[list] = None) -> None:
        self.heap = arr if arr else []
        if len(self.heap) >= 2:
            self._build(self.heap)

    def __str__(self) -> str:
        return self._dfs_traversal_str(0, 0)

    def __bool__(self) -> bool:
        return bool(self.heap)

    def _dfs_traversal_str(self, index: int, depth: int) -> str:
        if index is None or index >= len(self.heap):
            return ""
        left, right = self._get_children_idx(index)
        return (
            self._dfs_traversal_str(right, depth + 1)
            + "\n"
            + "    " * depth
            + str(self.heap[index]).rjust(10)
            + self._dfs_traversal_str(left, depth + 1)
        )

    def _build(self, arr: list) -> None:
        for i in range((len(arr) - 1) // 2, -1, -1):
            self._heapify_down(i)

    def _get_parent_idx(self, index: int) -> Optional[int]:
        if index <= 0:
            return None
        return (index - 1) // 2

    def _get_children_idx(self, index: int) -> tuple[Optional[int], Optional[int]]:
        left, right = 2 * index + 1, 2 * index + 2
        return (
            left if left < len(self.heap) else None,
            right if right < len(self.heap) else None,
        )

    def _heapify_up(self, index: int) -> None:
        if index is None or index <= 0:
            return
        parent_idx = self._get_parent_idx(index)
        if self.heap[parent_idx] > self.heap[index]:
            self.heap[parent_idx], self.heap[index] = (
                self.heap[index],
                self.heap[parent_idx],
            )
            self._heapify_up(parent_idx)

    def _heapify_down(self, index: int) -> None:
        if index is None or (children := self._get_children_idx(index)) == (None, None):
            return
        left, right = children
        if (left and right) and (
            self.heap[left] < self.heap[index] or self.heap[right] < self.heap[index]
        ):
            min_idx = left if self.heap[left] < self.heap[right] else right
            self.heap[index], self.heap[min_idx] = self.heap[min_idx], self.heap[index]
            self._heapify_down(min_idx)
        elif left and self.heap[left] < self.heap[index]:
            self.heap[index], self.heap[left] = self.heap[left], self.heap[index]
            self._heapify_down(left)

    def _check_heap_invariant(self) -> bool:
        for i in range(len(self.heap)):
            left, right = self._get_children_idx(i)
            if left and self.heap[left] < self.heap[i]:
                return False
            if right and self.heap[right] < self.heap[i]:
                return False
        return True

    def peek(self) -> Optional:
        """
        Returns the minimum element without removing it from the heap
        """
        return self.heap[0] if self.heap else None

    def insert(self, item) -> None:
        """
        Inserts the item into the heap and maintains the heap invariant property
        """
        self.heap.append(item)
        self._heapify_up(len(self.heap) - 1)

    def delete(self) -> Optional:
        """
        Removes the minimum element from the heap, returns it, and maintains the heap
        invariant property
        """
        if not self.heap:
            return None
        self.heap[0], self.heap[-1] = self.heap[-1], self.heap[0]
        min_element = self.heap.pop()
        if self.heap:
            self._heapify_down(0)
        return min_element
