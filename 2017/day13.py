from typing import List
import itertools
import time


def parse_input(lines: List[str]) -> dict[int, int]:
    layers = {}
    for line in lines:
        [layer, depth] = line.split(': ')
        layers[int(layer)] = int(depth)
    return layers


def scanner_is_at_the_top(time: int, depth: int) -> bool:
    period = 2 * depth - 2
    return time % period == 0


def calculate_severity(layers: dict[int, int], delay: int) -> int:
    severity = 0
    for layer in layers:
        depth = layers[layer]
        if scanner_is_at_the_top(layer + delay, depth):
            severity += layer * depth
    return severity


def to_remainder_constraints(layers: dict[int, int]) -> List[tuple[int, int]]:
    constraints = []
    for layer in layers:
        period = 2 * layers[layer] - 2
        remainder = (period - (layer % period)) % period
        constraints.append((period, remainder))
    return constraints


def satisfies_constraints(n: int, constraints: List[tuple[int, int]]) -> bool:
    for val, rem in constraints:
        if n % val == rem:
            return False
    return True


def part1(layers: dict[int, int]) -> int:
    return calculate_severity(layers, 0)


def part2(layers: dict[int, int]) -> int:
    delays = itertools.count()
    constraints = to_remainder_constraints(layers)
    return next(delay for delay in delays if satisfies_constraints(delay, constraints))


lines = open('input/day13.txt').readlines()
layers = parse_input(lines)

print('Part 1:', part1(layers))

start = time.time()
print('Part 2:', part2(layers))
print('Part 2 execution time:', time.time() - start, 'seconds')