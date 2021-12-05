from typing import Generator

MASK = int('1' * 16, 2)


def generator(initial: int, factor: int, multiple: int = 1) -> Generator[int, None, None]:
    prev = initial
    while True:
        next_val = prev * factor % 2147483647
        if next_val % multiple == 0:
            yield next_val
        prev = next_val


def count_matches(a_start: int, a_mult: int, b_start: int, b_mult: int, count: int) -> int:
    A = generator(a_start, 16807, a_mult)
    B = generator(b_start, 48271, b_mult)
    matched = 0

    for i in range(count):
        a = next(A)
        b = next(B)
        if a & MASK == b & MASK:
            matched += 1

        if i % 1000000 == 0:
            print('Iteration', i)

    return matched


def part1(a_start: int, b_start: int) -> int:
    return count_matches(a_start, 1, b_start, 1, int(4 * 1E7))


def part2(a_start: int, b_start: int) -> int:
    return count_matches(a_start, 4, b_start, 8, int(5 * 1E6))


print('Part 1:', part1(618, 814))
print('Part 2:', part2(618, 814))
