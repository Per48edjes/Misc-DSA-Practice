class UnionFind:
    def __init__(self, n):
        self.label = [i for i in range(n)]
        self.size = [1 for _ in range(n)]

    def find(self, x) -> int:
        if self.label[x] == x:
            return x
        else:
            self.label[x] = self.find(self.label[x])
            return self.label[x]

    def union(self, x, y):
        x_group = self.find(x)
        y_group = self.find(y)
        if self.size[x_group] > self.size[y_group]:
            self.label[y_group] = x_group
            self.size[x_group] += self.size[y_group]
        else:
            self.label[x_group] = y_group
            self.size[y_group] += self.size[x_group]
