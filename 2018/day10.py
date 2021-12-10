from typing import Iterable, NamedTuple
import re
from itertools import count
from collections import deque
from core import run


class Vector(NamedTuple):
    x: int
    y: int


class Point(NamedTuple):
    position: Vector
    velocity: Vector

    def position_at(self, time: int) -> Vector:
        return Vector(self.position.x + time * self.velocity.x, self.position.y + time * self.velocity.y)


def parse_input(lines: list[str]) -> list[Point]:
    pattern = "position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>"
    return [Point(Vector(int(px), int(py)), Vector(int(vx), int(vy))) for px, py, vx, vy in (re.match(pattern, line).groups() for line in lines)]


def adjacent_to(pos: Vector) -> Iterable[Vector]:
    yield Vector(pos.x + 1, pos.y)
    yield Vector(pos.x - 1, pos.y)
    yield Vector(pos.x, pos.y + 1)
    yield Vector(pos.x, pos.y - 1)
    yield Vector(pos.x + 1, pos.y + 1)
    yield Vector(pos.x + 1, pos.y - 1)
    yield Vector(pos.x - 1, pos.y - 1)
    yield Vector(pos.x - 1, pos.y + 1)


def get_adjacent_positions(all: set[Vector]) -> tuple[set[Vector], set[Vector]]:
    position = all.pop()
    adjacent = set()
    queue = deque([position])
    while queue:
        current = queue.popleft()
        if current not in adjacent:
            adjacent.add(current)
            for nxt in (nxt for nxt in adjacent_to(current) if nxt in all):
                queue.append(nxt)
    return adjacent, all.difference(adjacent)


def shows_words(positions: list[Vector]) -> bool:
    remaining_positions = set(positions)
    max_y, min_y = None, None
    while remaining_positions:
        letter, remaining_positions = get_adjacent_positions(
            remaining_positions)
        letter_min_y = min(map(lambda v: v.y, letter))
        letter_max_y = max(map(lambda v: v.y, letter))
        if max_y is None:
            min_y, max_y = letter_min_y, letter_max_y
        elif min_y != letter_min_y or max_y != letter_max_y:
            return False
    return True


def print_word(positions: list[Vector], padding=3):
    start_x = min(map(lambda pos: pos.x, positions)) - padding
    end_x = max(map(lambda pos: pos.x, positions)) + padding
    start_y = min(map(lambda pos: pos.y, positions)) - padding
    end_y = max(map(lambda pos: pos.y, positions)) + padding
    height = end_y - start_y + 1
    width = end_x - start_x + 1
    transformed_points = set(
        map(lambda pos: (pos.x - start_x, pos.y - start_y), positions))
    for y in range(0, height):
        for x in range(0, width):
            print("#" if (x, y) in transformed_points else ".", end="")
        print()


def solve(lines: list[str]) -> int:
    points = parse_input(lines)
    for time in count(1):
        positions = [p.position_at(time) for p in points]
        if shows_words(positions):
            return positions, time


def part1(positions: list[Vector], _: int):
    print_word(positions)
    return "⬆⬆⬆"


run(part1, part2=lambda word, time: time, process_input=solve)
