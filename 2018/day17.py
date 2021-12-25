from typing import Iterator, TypeAlias
from core import fst, snd, run
import re


Point: TypeAlias = tuple[int, int]
Water: TypeAlias = set[Point]
Clay: TypeAlias = set[Point]
Boundaries: TypeAlias = tuple[Point, Point]


class State:
    clay: Clay
    running_water: Water
    still_water: Water
    origin: Point
    min_y: int
    max_y: int

    def __init__(self, clay: Clay) -> None:
        self.clay = clay
        self.running_water = set()
        self.still_water = set()
        self.origin = 500, 0
        self.min_y, self.max_y = 0, max(map(snd, clay))

    def is_sand(self, p: Point) -> bool:
        return p not in self.clay and p not in self.running_water and p not in self.still_water

    def is_in_bounds(self, p: Point) -> bool:
        _, y = p
        return self.min_y <= y <= self.max_y

    def is_water(self, p: Point) -> bool:
        return p in self.running_water or p in self.still_water

    def print(self):
        print('State:')
        xs = list(map(fst, self.clay))
        min_x, max_x = min(xs) - 1, max(xs) + 1
        for y in range(self.min_y, self.max_y + 1):
            for x in range(min_x, max_x + 1):
                char = '+' if (x, y) == self.origin else \
                    '|' if (x, y) in self.running_water else \
                    '~' if (x, y) in self.still_water else \
                    '#' if (x, y) in self.clay else \
                    '.'
                print(char, end='')
            print()


def create_point(c1: str, v1: int, c2: str, v2: int) -> Point:
    x = v1 if c1 == 'x' else v2
    y = v2 if c2 == 'y' else v1
    return x, y


def parse_points(line: str) -> Iterator[Point]:
    c1, c1v1, c2, c2v1, c2v2 = re.match(
        r"(x|y)=(\d+), (x|y)=(\d+)\.\.(\d+)", line).groups()
    v1 = int(c1v1)
    for v2 in range(int(c2v1), int(c2v2) + 1):
        yield create_point(c1, v1, c2, v2)


def parse(lines: list[str]) -> list[Point]:
    points: list[Point] = []
    for line in lines:
        for point in parse_points(line):
            points.append(point)
    return points


def flow_down(state: State, start: Point):
    x, y = start
    while state.is_sand((x, y)) and state.is_in_bounds((x, y)):
        state.running_water.add((x, y))
        y += 1
    if state.is_in_bounds((x, y)) and (x, y) not in state.running_water:
        next_sinks = spread(state, (x, y - 1))
        for sink in next_sinks:
            if not state.is_water(sink):
                flow_down(state, sink)


def spread(state: State, start: Point) -> list[Point]:
    x, y = start
    while True:
        state.running_water.add((x, y))
        left_x, left_sink = spread_until_sink(state, (x, y), -1)
        right_x, right_sink = spread_until_sink(state, (x, y), 1)
        sinks = [sink for sink in (left_sink, right_sink) if sink is not None]
        if not sinks:
            for still_x in range(left_x, right_x + 1):
                state.running_water.remove((still_x, y))
                state.still_water.add((still_x, y))
            y -= 1
        else:
            return sinks


def spread_until_sink(state: State, start: Point, dx: int) -> tuple[int | None, Point | None]:
    x, y = start
    x += dx
    while (state.is_sand((x, y)) or state.is_water((x, y))) and not state.is_sand((x, y + 1)):
        state.running_water.add((x, y))
        x += dx
    if state.is_sand((x, y + 1)):
        return None, (x, y)
    else:
        return x - dx, None


def solve(lines: list[str]) -> State:
    state = State(set(parse(lines)))
    flow_down(state, state.origin)
    state.running_water.discard(state.origin)
    return state


def part1(state: State) -> int:
    min_clay_y = min(map(snd, state.clay))
    return len([1 for _, y in list(state.running_water) + list(state.still_water) if y >= min_clay_y])


def part2(state: State) -> int:
    return len(state.still_water)


run(part1, part2, process_input=solve)
