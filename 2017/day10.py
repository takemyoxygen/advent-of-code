from typing import List

from common.knot_hash import Circle, hash_round, get_knot_hash


def part1(circle: Circle, lengths: List[int]) -> int:
    hash_round(circle, lengths)
    return circle[0] * circle[1]


def part2(lengths_string: str):
    return get_knot_hash(lengths_string)


print(part1(Circle(range(256)), [129, 154, 49, 198, 200, 133, 97, 254, 41, 6, 2, 1, 255, 0, 191, 108]))
print(part2('129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108'))
