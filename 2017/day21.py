from typing import List, Iterator, Iterable, TypeVar
from itertools import accumulate
import functools
import operator

Grid = List[List[str]]
Rules = dict[str, str]
T = TypeVar('T')


def create_empty(size: int, initial: T = '') -> List[List[T]]:
    return [[initial] * size for _ in range(size)]


def rotate(grid: Grid) -> Grid:
    size = len(grid)
    rotated = create_empty(size)
    for i in range(size):
        for j in range(size):
            rotated[i][j] = grid[size - j - 1][i]
    return rotated


def flip_horizontal(grid: Grid) -> Grid:
    return grid[::-1]


def flip_vertical(grid: Grid) -> Grid:
    return [line[::-1] for line in grid]


def flip_diagonal(grid: Grid) -> Grid:
    n = len(grid)
    flipped = create_empty(n)
    for i in range(n):
        for j in range(n):
            flipped[i][j] = grid[j][i]
    return flipped


def flip_antidiagonal(grid: Grid) -> Grid:
    n = len(grid)
    flipped = create_empty(n)
    for i in range(n):
        for j in range(n):
            flipped[i][j] = grid[n - j - 1][n - i - 1]
    return flipped


def variations(grid: Grid) -> Iterator[Grid]:
    yield from accumulate(range(3), lambda s, i: rotate(s), initial=grid)
    yield flip_vertical(grid)
    yield flip_horizontal(grid)
    yield flip_diagonal(grid)
    yield flip_antidiagonal(grid)


def serialize_grid(grid: Grid) -> str:
    return '/'.join(map(lambda row: ''.join(row), grid))


def deserialize_grid(s: str) -> Grid:
    return [[char for char in row] for row in s.split('/')]


def apply_rule(grid: Grid, rules: Rules) -> Grid:
    encoded = map(serialize_grid, variations(grid))
    matched_rules = set((var, rules[var]) for var in encoded if var in rules)
    if len(matched_rules) != 1:
        raise Exception(f'Expected 1 rule for {grid}, found: {matched_rules}')

    return deserialize_grid(matched_rules.pop()[1])


def split_grid(grid: Grid) -> List[List[Grid]]:
    n = 2 if len(grid) % 2 == 0 else 3
    size = len(grid) // n
    result = create_empty(size)
    for i in range(size):
        for j in range(size):
            result[i][j] = [row[j * n: (j + 1) * n] for row in grid[i * n: (i + 1) * n]]
    return result


def join_lists(lists: Iterable[List]) -> List:
    return functools.reduce(operator.add, lists)


def join_grid(grids: List[List[Grid]]) -> Grid:
    n = len(grids[0][0])
    size = len(grids)
    result = []
    for i in range(size):
        for sub_i in range(n):
            row = join_lists(map(lambda row: row[sub_i], grids[i]))
            result.append(row)
    return result


def step(grid: Grid, rules: Rules) -> Grid:
    sub_grids = split_grid(grid)
    processed = [[apply_rule(sub_grid, rules) for sub_grid in row] for row in sub_grids]
    return join_grid(processed)


def solve(initial: Grid, rules: Rules, steps: int) -> int:
    grid = initial
    for i in range(steps):
        print('step', i)
        grid = step(grid, rules)

    return len([char for row in grid for char in row if char == '#'])


lines = open('input/day21.txt').readlines()
rules = dict([tuple(map(lambda token: token.strip(), line.split('=>'))) for line in lines])

initial = deserialize_grid('.#./..#/###')

print('Part 1:', solve(initial, rules, 5))
print('Part 2:', solve(initial, rules, 18))
