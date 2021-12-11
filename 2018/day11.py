from typing import TypeAlias
import core


Grid: TypeAlias = list[list[int]]


def power_level(x: int, y: int, serial_number: int) -> int:
    rack_id = x + 10
    power = rack_id * y + serial_number
    power *= rack_id
    power = (power // 100) % 10
    return power - 5


def empty_grid() -> Grid:
    grid = []
    for i in range(0, 300):
        grid.append([None] * 300)
    return grid


def calc_power_sum_grid(serial_number: int, size: int, smaller_sum_grids: dict[int, Grid]) -> Grid:
    grid = empty_grid()
    coords = ((x, y) for x in range(1, 301 - size + 1)
              for y in range(1, 301 - size + 1))
    if size == 1:
        for x, y in coords:
            grid[y - 1][x - 1] = power_level(x, y, serial_number)
    else:
        for x, y in coords:
            grid[y - 1][x - 1] = smaller_sum_grids[size-1][y][x - 1] + \
                smaller_sum_grids[size-1][y - 1][x] - \
                (smaller_sum_grids[size - 2][y][x] if size - 2 in smaller_sum_grids else 0) + \
                smaller_sum_grids[1][y - 1][x - 1] + \
                smaller_sum_grids[1][y + size - 2][x + size - 2]
    return grid


def find_max(grid: Grid, size: int) -> tuple[int, int, int]:
    x, y = max(((x, y) for x in range(1, 301 - size + 1)
               for y in range(1, 301 - size + 1)), key=lambda p: grid[p[1] - 1][p[0] - 1])
    return x, y, grid[y - 1][x - 1]


def part1(serial_number: int):
    sum_grids_per_size = {}
    for size in range(1, 4):
        sum_grids_per_size[size] = calc_power_sum_grid(
            serial_number, size, sum_grids_per_size)
    x, y, _ = find_max(sum_grids_per_size[3], 3)
    return f'{x},{y}'


def part2(serial_number: int):
    sizes = range(1, 301)
    max_per_size = {}
    sum_grids_per_size = {}

    for size in sizes:
        grid = calc_power_sum_grid(serial_number, size, sum_grids_per_size)
        sum_grids_per_size[size] = grid
        max_per_size[size] = find_max(grid, size)

        if max_per_size[size][2] < 0:
            break

    max_size = max(max_per_size, key=lambda k: max_per_size[k][2])
    x, y, _ = max_per_size[max_size]
    return f'{x},{y},{max_size}'


core.run(part1, part2, process_input=lambda lines: int(lines[0]))
