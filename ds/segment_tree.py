class SegTree:
    def __init__(self, arr: list[int], op, default) -> None:
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
