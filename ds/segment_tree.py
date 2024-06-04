import math


class SegmentTree4N:
    def __init__(self, arr: list[int], op=min, default=math.inf) -> None:
        self.n = len(arr)
        self.default, self.op = default, op
        self.tree = [default] * (4 * self.n)
        self._build(arr, 1, 0, self.n - 1)

    # NOTE: tl, tr is the spanning range of vertex v
    def _build(self, arr: list[int], v: int, tl: int, tr: int) -> None:
        if tl == tr:
            self.tree[v] = arr[tl]
        else:
            tm = (tl + tr) // 2
            self._build(arr, 2 * v, tl, tm)
            self._build(arr, 2 * v + 1, tm + 1, tr)
            self.tree[v] = self.op(self.tree[2 * v], self.tree[2 * v + 1])

    def _update(self, v: int, tl: int, tr: int, i: int, val: int) -> None:
        if tl == tr:
            self.tree[v] = val
        else:
            tm = (tl + tr) // 2
            if i <= tm:
                self._update(2 * v, tl, tm, i, val)
            else:
                self._update(2 * v + 1, tm + 1, tr, i, val)
            self.tree[v] = self.op(self.tree[2 * v], self.tree[2 * v + 1])

    def update(self, i, val) -> None:
        return self._update(1, 0, self.n - 1, i, val)

    # NOTE: l, r is the query range
    def _query(self, v: int, tl: int, tr: int, l: int, r: int) -> int | float:
        if tr < l or r < tl:
            return self.default
        if l <= tl and tr <= r:
            return self.tree[v]
        tm = (tl + tr) // 2
        left = self._query(2 * v, tl, tm, l, min(r, tm))
        right = self._query(2 * v + 1, tm + 1, tr, max(l, tm + 1), r)
        return self.op(left, right)

    # NOTE: Query is inclusive of l and inclusive of r, i.e., [l, r]
    def query(self, l: int, r: int) -> int | float:
        return self._query(1, 0, self.n - 1, l, r)


class SegmentTree2N:
    def __init__(self, arr: list[int], op=min, default=math.inf) -> None:
        self.default, self.op = default, op
        self.n = len(arr)
        self.tree = [default] * (2 * (self.n))
        self.tree[-self.n :] = arr
        for i in reversed(range(1, self.n)):
            left, right = self.tree[2 * i], self.tree[2 * i + 1]
            self.tree[i] = self.op(left, right)

    def __str__(self) -> str:
        return str(self.tree)

    def update(self, i, val) -> None:
        i += self.n
        self.tree[i] = val
        while i > 1:
            i = i // 2
            left, right = self.tree[2 * i], self.tree[2 * i + 1]
            self.tree[i] = self.op(left, right)

    # NOTE: Query is inclusive of l and exclusive of r, i.e., [l, r)
    def query(self, l: int, r: int) -> int:
        l += self.n
        r += self.n
        res = self.default
        while l < r:
            if l % 2 == 1:
                res = self.op(self.tree[l], res)
                l += 1
            if r % 2 == 1:
                r -= 1
                res = self.op(self.tree[r], res)
            l //= 2
            r //= 2
        return res
