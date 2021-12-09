from typing import NamedTuple, TypeAlias
import re
from core import snd, replace_at, last
from collections import defaultdict


class Point(NamedTuple):
    label: str
    x: int
    y: int


def parse_input(lines: list[str]) -> list[Point]:
    return [
        Point('x-y', int(x), int(y)) for x, y in
        (re.match("^(\d+), (\d+)$", line).groups() for line in lines)
    ]


def manhattan(p1: Point, x: int, y: int) -> int:
    return abs(p1.x - x) + abs(p1.y - y)


def get_closest_point(x: int, y: int, points: list[Point]) -> Point | None:
    points_with_distances = list(
        map(lambda p: (p, manhattan(p, x, y)), points))
    closest, distance_to_closest = min(points_with_distances, key=snd)

    if distance_to_closest == 0:
        return None

    if len(list(filter(lambda pd: pd[1] == distance_to_closest, points_with_distances))) > 1:
        return None

    return closest


def mark_area(points: list[Point]) -> list[str]:
    max_x, max_y = max(map(lambda p: p.x, points)), max(
        map(lambda p: p.y, points))
    lines = []
    for y in range(0, max_y + 1):
        line = ''
        for x in range(0, max_x + 1):
            closest = get_closest_point(x, y, points)
            line += closest.label.lower() if closest is not None else '.'
        lines.append(line)

    for point in points:
        lines[point.y] = replace_at(lines[point.y], point.x, point.label)

    return lines


def part1(points: list[Point]):
    by_x = sorted(points, key=lambda p: p.x)
    by_y = sorted(points, key=lambda p: p.y)


    start_x = by_x[0].x  - (by_x[1].x - by_x[0].x)
    start_y = by_y[0].y  - (by_y[1].y - by_y[0].y)
    end_x = last(by_x).x + (last(by_x).x - by_x[len(by_x) - 2].x)
    end_y = last(by_y).y + (last(by_y).y - by_y[len(by_y) - 2].y)

    closest_count = defaultdict(lambda: 0)

    for x in range(start_x, end_x + 1):
        for y in range(start_y, end_y + 1):
            closest = get_closest_point(x, y, points)
            if closest:
                closest_count[closest.label] += 1

    perimeter = [(x, y) for x in range(start_x, end_x + 1) for y in (start_y, end_y)]

    for x in range(start_x, end_x + 1):
        for y in [start_y, end_y]:


def print_area(points: list[Point]) -> None:
    for line in mark_area(points):
        print(line)


print_area([Point('A', 3, 8), Point('B', 5, 2)])
