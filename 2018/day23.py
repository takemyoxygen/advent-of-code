import sys
import re
from typing import Callable, TypeAlias, Any
from core import run, snd, fst
import heapq


Vector: TypeAlias = tuple[int, int, int]
Nanobot: TypeAlias = tuple[Vector, int]
Parallelepiped: TypeAlias = tuple[Vector, Vector]


def parse(lines: list[str]) -> list[Nanobot]:
    output = list[tuple[tuple[int, int, int], int]]()
    for line in lines:
        [x, y, z, r] = list(map(int, re.findall(r'-?\d+', line)))
        output.append(((x, y, z), r))
    return output


def in_range(n1: Nanobot, point: Vector) -> bool:
    coords, radius = n1
    dist = sum(map(lambda c1, c2: abs(c1 - c2), coords, point))
    return dist <= radius


def part1(nanobots: list[Nanobot]) -> Any:
    strongest = max(nanobots, key=snd)
    return len(list(filter(lambda n: in_range(strongest, n[0]), nanobots)))


def nano_intercects_para(nanobot: Nanobot, para: Parallelepiped) -> bool:
    (nx, ny, nz), nr = nanobot
    (x1, y1, z1), (x2, y2, z2) = para

    d = 0
    if nx < x1:
        d += x1 - nx
    elif nx > x2:
        d += nx - x2

    if ny < y1:
        d += y1 - ny
    elif ny > y2:
        d += ny - y2

    if nz < z1:
        d += z1 - nz
    elif nz > z2:
        d += nz - z2

    return d <= nr


def split_coord(c_min: int, c_max: int) -> list[tuple[int, int]]:
    if c_min == c_max:
        return [(c_min, c_min)]

    mid = (c_min + c_max) // 2
    return [(c_min, mid), (mid + 1, c_max)]


def split_para(para: Parallelepiped) -> list[Parallelepiped]:
    (x_min, y_min, z_min), (x_max, y_max, z_max) = para
    return [((x1, y1, z1), (x2, y2, z2))
            for (x1, x2) in split_coord(x_min, x_max)
            for (y1, y2) in split_coord(y_min, y_max)
            for (z1, z2) in split_coord(z_min, z_max)]


def find_coord_range2(coord: Callable[[tuple[int, int, int]], int], nanobots: list[Nanobot]) -> tuple[int, int]:
    mapped = list(map(lambda nano: coord(nano[0]), nanobots))
    return min(mapped), max(mapped)


def part2(nanobots: list[Nanobot]) -> int:
    x_min, x_max = find_coord_range2(fst, nanobots)
    y_min, y_max = find_coord_range2(snd, nanobots)
    z_min, z_max = find_coord_range2(lambda x: x[2], nanobots)
    start_para = (x_min, y_min, z_min), (x_max, y_max, z_max)
    queue: list[tuple[int, int, Parallelepiped]] = [
        (0, sys.maxsize, start_para)]

    while True:
        _, dist, para = heapq.heappop(queue)

        if para[0] == para[1]:
            return dist

        for sub_para in split_para(para):
            nanos_touched = len(
                list(filter(lambda nano: nano_intercects_para(nano, sub_para), nanobots)))
            dist = sum(
                map(abs, sub_para[0])) if sub_para[0] == sub_para[1] else sys.maxsize
            heapq.heappush(queue, (-nanos_touched, dist, sub_para))


run(part1, part2, process_input=parse)
