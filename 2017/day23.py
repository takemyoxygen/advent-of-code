from os import WUNTRACED
from typing import List
from collections import defaultdict

from common.registers import handlers, Registers


def part1(instructions: List[List[str]]) -> int:
    registers = defaultdict(lambda: 0)
    mul_count = 0
    position = 0
    while 0 <= position < len(instructions):
        [name, *args] = instructions[position]
        if name == 'mul':
            mul_count += 1

        position = handlers[name](registers, position, *args)

    return mul_count


def is_composite(x: int) -> bool:
    for i in range(2, x):
        if x % i == 0:
            return True
    return False

def part2_code_optimized():
    b = 109900
    c = 126900
    h = 0

    while True:
        if is_composite(b):
            h += 1
        if b == c:
            return h
        b += 17


instructions = [line.rstrip('\n').split(' ') for line in open('input/day23.txt').readlines()]

print('Part 1:', part1(instructions))
print('Part 2', part2_code_optimized())
