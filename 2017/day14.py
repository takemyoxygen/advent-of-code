from typing import List, Generator
from common.knot_hash import get_knot_hash


SIZE = 128


class DSU:
    def __init__(self):
        self.__parents = []

    def create_new(self):
        x = len(self.__parents)
        self.__parents.append(x)
        return x

    def root_of(self, x: int) -> int:
        current = x
        while self.__parents[current] != current:
            current = self.__parents[current]
        return current

    def merge(self, x: int, y: int) -> int:
        y_root = self.root_of(y)
        x_root = self.root_of(x)
        if y_root != x_root:
            self.__parents[y_root] = x_root
        return x_root

    def disjoint_count(self):
        count = 0
        for i in range(len(self.__parents)):
            if self.__parents[i] == i:
                count += 1
        return count

    def __str__(self):
        return str(self.__parents)


def adjacent_before(i: int, j: int) -> Generator[tuple[int, int], None, None]:
    if i > 0:
        yield i - 1, j
    if j > 0:
        yield i, j - 1


def get_row_binary(prefix: str, row: int) -> str:
    s = f'{prefix}-{row}'
    knot_hash = get_knot_hash(s)
    return ''.join(format(x, '08b') for x in bytes.fromhex(knot_hash))


def count_ones(s: str) -> int:
    return len(list(filter(lambda c: c == '1', s)))


def create_grid(prefix: str) -> List[str]:
    return [get_row_binary(prefix, row) for row in range(SIZE)]


def part1(grid: List[str]):
    return sum(map(count_ones, grid))


def part2(grid: List[str]):
    components = []
    dsu = DSU()
    for i in range(len(grid)):
        components.append([-1] * SIZE)
        for j in range(len(grid[i])):
            if grid[i][j] == '1':
                adjacent_components = set(components[adj_i][adj_j]
                                          for adj_i, adj_j in adjacent_before(i, j)
                                          if grid[adj_i][adj_j] == '1')
                if len(adjacent_components) == 0:
                    components[i][j] = dsu.create_new()
                elif len(adjacent_components) == 1:
                    components[i][j] = adjacent_components.pop()
                else:
                    [c1, c2] = list(adjacent_components)
                    components[i][j] = dsu.merge(c1, c2)

    return dsu.disjoint_count()


puzzle_input = 'jxqlasbh'
# puzzle_input = 'flqrgnkx'

grid = create_grid(puzzle_input)

print('Part 1:', part1(grid))
print('Part 2:', part2(grid))
