from typing import TypeAlias
from core import run
from collections.abc import Iterator

Point: TypeAlias = tuple[int, int, int, int]

def parse(lines: list[str]) -> list[Point]:
    return [tuple(map(int, l.split(','))) for l in lines]


def dist(p1: Point, p2: Point) -> int:
    return sum(map(lambda c1, c2: abs(c1 - c2), p1, p2))


class DSU:
    parents: list[int]

    def __init__(self, size: int) -> None:
        self.parents = []
        for i in range(size):
            self.parents.append(i)

    def root_of(self, x: int) -> int:
        p = x
        while p != self.parents[p]:
            p = self.parents[p]
        return p

    def connect(self, x1: int, x2: int) -> None:
        p1 = self.root_of(x1)
        p2 = self.root_of(x2)
        self.parents[p1] = p2

    def roots(self) -> Iterator[int]:
        for x in range(len(self.parents)):
            if self.parents[x] == x:
                yield x


def part1(points: list[Point]) -> int:
    dsu = DSU(len(points))

    for i in range(len(points) - 1):
        for j in range(i + 1, len(points)):
            if dist(points[i], points[j]) <= 3:
                dsu.connect(i, j)

    return len(list(dsu.roots()))


run(part1, process_input=parse)