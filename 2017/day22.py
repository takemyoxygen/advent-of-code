from typing import List, Callable
from enum import Enum


class State(Enum):
    CLEAN = 0
    WEAKENED = 1
    INFECTED = 2
    FLAGGED = 3


Vector = tuple[int, int]

directions = [
    (-1, 0),
    (0, -1),
    (1, 0),
    (0, 1),
]


def reverse_direction(direction: Vector) -> Vector:
    return -direction[0], -direction[1]


def change_direction(direction: Vector, change: int) -> Vector:
    current = directions.index(direction)
    return directions[(current + change) % len(directions)]


def turn_left(direction: Vector) -> Vector:
    return change_direction(direction, 1)


def turn_right(direction: Vector) -> Vector:
    return change_direction(direction, -1)


def add_vec(v1: Vector, v2: Vector) -> Vector:
    return v1[0] + v2[0], v1[1] + v2[1]


def burst(dirty: dict[Vector, State], position: Vector, direction: Vector, handler: Callable[[State, Vector], tuple[State, Vector]]) -> tuple[bool, Vector, Vector]:
    caused_infection = False
    current_state = dirty[position] if position in dirty else State.CLEAN
    next_state, next_dir = handler(current_state, direction)

    if next_state == State.CLEAN:
        del dirty[position]
    else:
        dirty[position] = next_state

    if next_state == State.INFECTED:
        caused_infection = True

    position = add_vec(position, next_dir)

    return caused_infection, next_dir, position


def part1_handler(state: State, direction: Vector) -> tuple[State, Vector]:
    return (State.CLEAN, turn_right(direction)) if state == State.INFECTED else (State.INFECTED, turn_left(direction))


def part2_handler(state: State, direction: Vector) -> tuple[State, Vector]:
    if state == State.CLEAN:
        return State.WEAKENED, turn_left(direction)
    elif state == State.WEAKENED:
        return State.INFECTED, direction
    elif state == State.INFECTED:
        return State.FLAGGED, turn_right(direction)
    else:
        return State.CLEAN, reverse_direction(direction)


def solve(dirty: dict[Vector, State], position: Vector, bursts: int, handler: Callable[[State, Vector], tuple[State, Vector]]) -> int:
    direction = directions[0]
    infected_nodes = 0
    for i in range(bursts):
        caused_infection, direction, position = burst(dirty, position, direction, handler)
        if caused_infection:
            infected_nodes += 1
    return infected_nodes


def part1(dirty: dict[Vector, State], position: Vector, bursts: int) -> int:
    return solve(dirty, position, bursts, part1_handler)


def part2(dirty: dict[Vector, State], position: Vector, bursts: int) -> int:
    return solve(dirty, position, bursts, part2_handler)


def parse_input(lines: List[str]) -> tuple[dict[Vector, State], Vector]:
    dirty = dict()

    for i in range(len(lines)):
        for j in range(len(lines[i])):
            if lines[i][j] == '#':
                dirty[(i, j)] = State.INFECTED

    position = (len(lines)) // 2, (len(lines[0])) // 2

    return dirty, position


lines = [line.strip() for line in open('input/day22.txt').readlines()]
infected, position = parse_input(lines)
print('Initial position', position)

print('Part 1', part1(infected.copy(), position, 10000))
print('Part 1', part2(infected.copy(), position, 10000000))