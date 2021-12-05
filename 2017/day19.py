from typing import List, Union

Diagram = List[str]
Direction = tuple[int, int]
Point = tuple[int, int]


directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]


def step(pos: Point, direction: Direction) -> Point:
    return pos[0] + direction[0], pos[1] + direction[1]


def element_at(diagram: Diagram, pos: Point) -> Union[str, None]:
    try:
        return diagram[pos[0]][pos[1]]
    except IndexError:
        return None


def next_dir(diagram: Diagram, current_pos: Point, current_dir: Direction) -> Union[Direction, None]:
    reverse_dir: Direction = tuple(map(lambda x: -x, current_dir))
    prev_pos = step(current_pos, reverse_dir)
    for direction in directions:
        next_pos = step(current_pos, direction)
        next_char = element_at(diagram, next_pos)
        if next_pos != prev_pos and next_char and not next_char.isspace():
            return direction
    return None


def solve(diagram: Diagram) -> tuple[str, int]:
    position = 0, diagram[0].index('|')
    current_dir = 1, 0
    path = ''
    steps = 1
    while current_dir:
        next_position = step(position, current_dir)
        next_char = element_at(diagram, next_position)
        if not next_char or next_char.isspace():
            if element_at(diagram, position) == '+':
                current_dir = next_dir(diagram, position, current_dir)
            else:
                current_dir = None
        elif next_char.isalpha():
            steps += 1
            path += next_char
            position = next_position
        else:
            steps += 1
            position = next_position
    return path, steps


diagram = [line.rstrip('\n') for line in open('input/day19.txt').readlines()]
path, steps = solve(diagram)
print('Part 1:', path)
print('Part 2:', steps)