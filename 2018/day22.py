from typing import Iterator, TypeAlias
from core import empty_grid, run
import sys
import collections
import heapq
import math
import re


def get_geo_index(x: int, y: int, target: tuple[int, int], erosion_levels: list[list[int]]) -> int:
    match x, y:
        case 0, 0:
            return 0
        case x, 0:
            return x * 16807
        case 0, y:
            return y * 48271
        case p if p == target:
            return 0
        case _:
            return erosion_levels[x - 1][y] * erosion_levels[x][y - 1]


def get_erosion_levels(size: int, target: tuple[int, int], depth: int) -> list[list[int]]:
    erosion_levels = empty_grid(size, size, 0)
    for x in range(0, size):
        for y in range(0, size):
            erosion_levels[x][y] = (get_geo_index(
                x, y, target, erosion_levels) + depth) % 20183
    return erosion_levels


def get_risk_level(erosion_levels: list[list[int]], target: tuple[int, int]) -> int:
    cols, rows = target
    return sum(x % 3 for row in erosion_levels[:cols + 1] for x in row[:rows + 1])


regions = ['rocky', 'wet', 'narrow']


def get_region_type(erosion_levels: list[list[int]], x: int, y: int) -> str:
    try:
        return regions[erosion_levels[x][y] % 3]
    except IndexError as e:
        print('Index error', x, y)
        raise e


region_to_tools = {
    'rocky': set(['gear', 'torch']),  # .
    'wet': set(['gear', 'neither']),  # =
    'narrow': set(['torch', 'neither'])  # |
}

Node: TypeAlias = tuple[int, int, str]  # x, y, tool
Edge: TypeAlias = tuple[float, Node]  # time, destination

END: Node = sys.maxsize, sys.maxsize, 'torch'


def get_edges(erosion_levels: list[list[int]], node: Node, target: tuple[int, int]) -> Iterator[Edge]:
    x, y, tool = node

    if (x, y) == (END[0], END[1]):
        return

    allowed_tools = region_to_tools[get_region_type(erosion_levels, x, y)]
    adjacent_regions = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

    for adj_x, adj_y in adjacent_regions:
        if adj_x >= 0 and adj_y >= 0 and adj_x < len(erosion_levels) and adj_y < len(erosion_levels[adj_x]):
            adj_type = get_region_type(erosion_levels, adj_x, adj_y)
            adj_tools = region_to_tools[adj_type]
            common_tools = allowed_tools.intersection(adj_tools)
            for common_tool in common_tools:
                time = 1 if common_tool == tool else 8
                yield time, (adj_x, adj_y, common_tool)

    if (x, y) == target:
        yield 0 if tool == END[2] else 7, END


def dijsktra(erosion_levels: list[list[int]], target: tuple[int, int]) -> float:
    start = 0, 0, 'torch'
    queue: list[Edge] = [(0, start)]
    times = collections.defaultdict[Node, float](lambda: math.inf)
    times[start] = 0

    while queue:
        time_to_node, node = heapq.heappop(queue)

        if time_to_node != times[node]:
            continue

        if times[END] < time_to_node:
            return times[END]

        for next_time, next_node in get_edges(erosion_levels, node, target):
            time = time_to_node + next_time
            if times[next_node] > time:
                times[next_node] = time
                heapq.heappush(queue, (time, next_node))

    return math.inf


def print_regions(erosion_levels: list[list[int]]):
    def get_char(x: int, y: int):
        match get_region_type(erosion_levels, x, y):
            case 'rocky': return '.'
            case 'wet': return '='
            case 'narrow': return '|'
            case _: return 'x'

    for y in range(0, len(erosion_levels[0])):
        for x in range(0, len(erosion_levels)):
            print(get_char(x, y), end='')
        print()


def calculate_erosion_levels(lines: list[str]) -> tuple[list[list[int]], tuple[int, int]]:
    [depth] = list(map(int, re.findall(r'\d+', lines[0])))
    target = tuple(map(int, re.findall(r'\d+', lines[1])))
    size = max(target) * 2
    erosion_levels = get_erosion_levels(size, target, depth)
    return erosion_levels, target


def part1(erosion_levels: list[list[int]], target: tuple[int, int]) -> int:
    return get_risk_level(erosion_levels, target)


def part2(erosion_levels: list[list[int]], target: tuple[int, int]) -> float:
    return dijsktra(erosion_levels, target)


run(part1, part2, process_input=calculate_erosion_levels)
