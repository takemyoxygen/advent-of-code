from functools import reduce
from operator import xor
from typing import Iterable, List


class Circle:
    def __init__(self, values: Iterable[int]):
        self.__values = list(values)

    def normalize_index(self, index: int) -> int:
        return index % len(self.__values)

    def swap(self, i: int, j: int) -> None:
        i = self.normalize_index(i)
        j = self.normalize_index(j)
        self.__values[i], self.__values[j] = self.__values[j], self.__values[i]

    def __getitem__(self, item: int) -> int:
        return self.__values[self.normalize_index(item)]

    def __str__(self):
        return str(self.__values)

    def values(self) -> List[int]:
        return self.__values.copy()


def reverse(circle: Circle, start: int, length: int) -> None:
    end = circle.normalize_index(start + length - 1)
    for i in range(length // 2):
        circle.swap(start + i, end - i)


def hash_round(circle: Circle, lengths: List[int], position: int = 0, skip: int = 0) -> tuple[int, int]:
    for length in lengths:
        reverse(circle, position, length)
        position = circle.normalize_index(position + length + skip)
        skip += 1
    return position, skip


def hash_blocks(values: List[int]) -> List[int]:
    result = [0] * 16

    for i in range(16):
        start, end = i * 16, (i + 1) * 16
        result[i] = reduce(xor, values[start:end])

    return result


def hexify(x: int) -> str:
    hexed = hex(x)[2:]
    return hexed if len(hexed) == 2 else '0' + hexed


def get_knot_hash(s: str) -> str:
    circle = Circle(range(256))
    lengths = [ord(c) for c in s] + [17, 31, 73, 47, 23]
    position, skip = 0, 0
    for _ in range(64):
        position, skip = hash_round(circle, lengths, position, skip)

    hashed_blocks = hash_blocks(circle.values())
    return ''.join(map(hexify, hashed_blocks))