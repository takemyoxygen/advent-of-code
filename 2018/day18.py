from typing import TypeAlias
from collections.abc import Iterable
from collections import defaultdict
from core import run


Area: TypeAlias = list[list[str]]
Point: TypeAlias = tuple[int, int]


def adjacent_to(p: Point) -> Iterable[Point]:
    x, y = p
    yield x + 1, y
    yield x - 1, y
    yield x, y + 1
    yield x, y - 1
    yield x + 1, y + 1
    yield x + 1, y - 1
    yield x - 1, y + 1
    yield x - 1, y - 1


def in_bounds(area: Area, point: Point) -> bool:
    size = len(area)
    x, y = point
    return 0 <= x < size and 0 <= y < size


def get_adjacents_info(point: Point, area: Area) -> dict[str, int]:
    info = defaultdict[str, int](lambda: 0)
    for x, y in filter(lambda p: in_bounds(area, p), adjacent_to(point)):
        char = area[y][x]
        info[char] += 1
    return info


def get_next_state(area: Area, point: Point) -> str:
    x, y = point
    adjacents = get_adjacents_info(point, area)
    match area[y][x]:
        case '.':
            return '|' if adjacents['|'] >= 3 else '.'
        case '|':
            return '#' if adjacents['#'] >= 3 else '|'
        case '#':
            return '#' if adjacents['#'] >= 1 and adjacents['|'] >= 1 else '.'
        case x:
            raise Exception(f'Unknown location type {x}')


def new_area(size: int) -> Area:
    area: Area = []
    for _ in range(0, size):
        area.append(['.'] * size)
    return area


def get_next_area(area: Area) -> Area:
    size = len(area)
    next_area = new_area(size)
    for y in range(0, size):
        for x in range(0, size):
            next_area[y][x] = get_next_state(area, (x, y))
    return next_area


def count_chars(char: str, area: Area) -> int:
    return len([ch for row in area for ch in row if ch == char])


def part1(area: Area) -> int:
    for _ in range(10):
        area = get_next_area(area)
    return count_chars('|', area) * count_chars('#', area)


def parse(lines: list[str]) -> Area:
    return [list(line) for line in lines]


def print_area(area: Area):
    for row in area:
        print(''.join(row))


def part2(area: Area):
    by_minute = dict[int, int]()
    target = 1000000000
    limit = 500
    for i in range(1, limit + 1):
        area = get_next_area(area)
        by_minute[i] = count_chars('|', area) * count_chars('#', area)
    period = limit - \
        next(filter(lambda x: by_minute[limit] ==
             by_minute[x], range(limit - 1, 0, -1)))
    start = limit - period
    full_periods = (target - start) // period
    last_period_progress = target - (start + full_periods * period)
    return by_minute[start + last_period_progress]


run(part1, part2, process_input=parse)
