from collections import deque
from typing import Callable
import core


def should_react(c1: str, c2: str) -> bool:
    return c1.lower() == c2.lower() and abs(ord(c1) - ord(c2)) == 32


def process_polymer(input: str, ignore: Callable[[str], bool]) -> int:
    stack = deque()
    for char in filter(lambda char: not ignore(char), input):
        if stack and should_react(stack[0], char):
            stack.popleft()
        else:
            stack.appendleft(char)
    return len(stack)


def part1(input: str) -> int:
    return process_polymer(input, lambda _: False)


def part2(input: str) -> int:
    chars = map(chr, range(ord('a'), ord('z') + 1))

    def process_ignoring(ignore: str) -> int:
        lower = ignore.lower()
        upper = ignore.upper()
        return process_polymer(input, lambda char: char == lower or char == upper)

    return min(map(process_ignoring, chars))


core.run(part1, part2, process_input=lambda lines: lines[0])
