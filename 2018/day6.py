from typing import NamedTuple
import re
from core import snd, replace_at, last, run
from collections import defaultdict, deque
from bisect import bisect_left, bisect_right


class Point(NamedTuple):
    label: str
    x: int
    y: int


def parse_input(lines: list[str], options) -> tuple[list[Point], int]:
    points = [
        Point(f'{x}-{y}', int(x), int(y)) for x, y in
        (re.match("^(\d+), (\d+)$", line).groups() for line in lines)
    ]
    part2_limit = 32 if options.is_test else 10000
    return points, part2_limit


def manhattan(p1: Point, x: int, y: int) -> int:
    return abs(p1.x - x) + abs(p1.y - y)


def get_closest_point(x: int, y: int, points: list[Point]) -> Point | None:
    points_with_distances = list(
        map(lambda p: (p, manhattan(p, x, y)), points))
    closest, distance_to_closest = min(points_with_distances, key=snd)

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


def part1(points: list[Point], _: int) -> int:
    by_x = sorted(points, key=lambda p: p.x)
    by_y = sorted(points, key=lambda p: p.y)

    start_x = by_x[0].x - (by_x[1].x - by_x[0].x)
    start_y = by_y[0].y - (by_y[1].y - by_y[0].y)
    end_x = last(by_x).x + (last(by_x).x - by_x[len(by_x) - 2].x)
    end_y = last(by_y).y + (last(by_y).y - by_y[len(by_y) - 2].y)

    closest_count = defaultdict(lambda: 0)

    for x in range(start_x, end_x + 1):
        for y in range(start_y, end_y + 1):
            closest = get_closest_point(x, y, points)
            if closest:
                closest_count[closest.label] += 1

    perimeter = [(x, y) for x in range(start_x, end_x + 1) for y in (start_y, end_y)] + \
        [(x, y) for x in (start_x, end_x) for y in range(start_y, end_y + 1)]

    for x, y in perimeter:
        closest = get_closest_point(x, y, points)
        if closest and closest.label in closest_count:
            del closest_count[closest.label]

    return max(closest_count.values())


def total_distance(x: int, y: int, points: list[Point]) -> int:
    return sum(map(lambda p: manhattan(p, x, y), points))


def part2(points: list[Point], limit: int):
    xs = sorted(map(lambda p: p.x, points))
    ys = sorted(map(lambda p: p.y, points))

    start_x = xs[0]
    start_y = ys[0]
    end_x = last(xs)
    end_y = last(ys)

    init_x = (start_x + end_x) // 2
    init_y = (start_y + end_y) // 2
    init_d = total_distance(init_x, init_y, points)
    queue = deque([(init_x, init_y, init_d)])
    visited = set()
    while queue:
        x, y, d = queue.popleft()
        if (x, y) not in visited and d < limit:
            # right
            to_the_left = bisect_right(xs, x)
            new_d = d + to_the_left - (len(xs) - to_the_left)
            queue.append((x + 1, y, new_d))
            # left
            to_the_left = bisect_left(xs, x)
            new_d = d - to_the_left + (len(xs) - to_the_left)
            queue.append((x - 1, y, new_d))
            # down
            to_the_top = bisect_right(ys, y)
            new_d = d + to_the_top - (len(ys) - to_the_top)
            queue.append((x, y + 1, new_d))
            # up
            to_the_top = bisect_left(ys, y)
            new_d = d - to_the_top + (len(ys) - to_the_top)
            queue.append((x, y - 1, new_d))

            visited.add((x, y))

    return len(visited)


run(part1, part2, process_input=parse_input)
