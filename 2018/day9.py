from typing import Iterable
from functools import reduce
from collections import defaultdict
import re
import core


class Marble:
    value: int
    left: "Marble"
    right: "Marble"

    def __init__(self, value: int) -> None:
        self.value = value
        self.left = None
        self.right = None


def to_the_left(marble: Marble, steps: int):
    return reduce(lambda marble, _: marble.left, range(0, steps), marble)


def connect(left: Marble, right: Marble) -> None:
    left.right = right
    right.left = left


def remove(marble: Marble) -> tuple[Marble, Marble]:
    left = marble.left
    right = marble.right
    connect(left, right)
    return left, right


def player_moves(players_count: int) -> Iterable[int]:
    while True:
        for i in range(1, players_count + 1):
            yield i


def place_marble(current: Marble, value: int) -> tuple[Marble, int]:
    if value % 23 == 0:
        to_remove = to_the_left(current, 7)
        _, next_current = remove(to_remove)
        return next_current, value + to_remove.value

    new_marble = Marble(value)
    before = current.right
    after = before.right
    connect(before, new_marble)
    connect(new_marble, after)
    return new_marble, 0


def iterate_marbles(start: Marble) -> Iterable[int]:
    yield start.value
    current = start.right
    while current != start:
        yield current.value
        current = current.right


def parse_input(lines: list[str]) -> list[tuple[int, int]]:
    return [tuple(map(int, data)) for data in (re.match("(\d+) players; last marble is worth (\d+) points", line).groups() for line in lines)]


def part1(inputs: list[tuple[int, int]]):
    results = []

    for players_count, last_marble in inputs:

        scores = defaultdict(lambda: 0)

        current_marble = Marble(0)
        connect(current_marble, current_marble)

        for marble_value, player in zip(range(1, last_marble + 1), player_moves(players_count)):
            current_marble, score = place_marble(current_marble, marble_value)
            scores[player] += score

        best_score = max(scores.values())

        results.append(best_score)

    return results


def part2(inputs: list[tuple[int, int]]):
    [(players_count, last_marble)] = inputs
    return part1([(players_count, last_marble * 100)])


core.run(part1, part2, process_input=parse_input)
