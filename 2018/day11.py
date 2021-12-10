import core
import functools


@functools.cache
def power_level(x: int, y: int, serial_number: int) -> int:
    rack_id = x + 10
    power = rack_id * y + serial_number
    power *= rack_id
    power = (power // 100) % 10
    return power - 5


@functools.cache
def find_square_power(x: int, y: int, size: int, serial_number: int) -> int:
    return sum(power_level(x + dx, y + dy, serial_number) for dx in range(0, size) for dy in range(0, size))


def find_max_power_square(serial_number: int, size: int) -> tuple[int, int, int]:
    coords = ((x, y) for x in range(1, 301 - size) for y in range(1, 301 - size))
    with_power = ((x, y, find_square_power(x, y, size, serial_number)) for x, y in coords)
    return max(with_power, key=lambda x: x[2])


def part1(serial_number: int) -> int:
    return find_max_power_square(serial_number, 3)


def part2(serial_number: int) -> int:
    with_max_power = ((x, y, size, power) for size, (x, y, power) in ((size, find_max_power_square(serial_number, size)) for size in range(1, 301)))
    return max(with_max_power, key=lambda x: x[3])


core.run(part1, part2, process_input=lambda lines: int(lines[0]))
