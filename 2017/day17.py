from typing import List


def perform_inserts(steps: int, inserts: int) -> List[int]:
    circle = [0]
    pos = 0
    for i in range(1, inserts + 1):
        pos = (pos + steps) % len(circle) + 1
        circle.insert(pos, i)
    return circle


def part1(steps: int) -> int:
    target = 2017
    circle = perform_inserts(steps, target)
    index = circle.index(target) + 1
    return circle[index % len(circle)]


def part2(steps: int) -> int:
    target = 50000000
    after_zero = None
    pos = 0
    size = 1
    for i in range(1, target + 1):
        pos = (pos + steps) % size + 1
        if pos == 1:
            after_zero = i
        size += 1
    return after_zero


steps = 324
print('Part 1:', part1(steps))
print('Part 2:', part2(steps))
