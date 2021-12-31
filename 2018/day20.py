from collections import deque, defaultdict
from typing import Iterable, TypeAlias
from core import next_id, fst, snd, run
import math

Position: TypeAlias = tuple[int, int]  # x, y


class Node:
    id: int
    direction: str | None
    next: set["Node"]

    def __init__(self, direction: str | None = None) -> None:
        self.id = next_id()
        self.direction = direction
        self.next = set()

    def __hash__(self) -> int:
        return self.id

    def connect_with(self, node: "Node") -> None:
        self.next.add(node)


def connect(prev: Iterable[Node], node: Node) -> None:
    for p in prev:
        p.connect_with(node)


def read_dirs(prev: set[Node], chars: deque[str]) -> set[Node]:
    nodes = list[Node]()
    while chars and chars[0].isalpha():
        dir = chars.popleft()
        node = Node(dir)
        if nodes:
            nodes[-1].connect_with(node)
        nodes.append(node)
    connect(prev, nodes[0])
    return set([nodes[-1]])


def read_branch(prev: set[Node], chars: deque[str]) -> set[Node]:
    match chars[0]:
        case ')':
            return prev
        case '|':
            chars.popleft()
            return prev if chars[0] == ')' else read_expr(prev, chars)
        case _:
            return read_expr(prev, chars)


def read_branches(prev: set[Node], chars: deque[str]) -> set[Node]:
    chars.popleft()  # (
    ends = set[Node]()
    while chars[0] != ')':
        branch_ends = read_branch(prev, chars)
        ends = ends.union(branch_ends)
    chars.popleft()  # )
    return ends


def read_expr(prev: set[Node], chars: deque[str]) -> set[Node]:
    while chars:
        match chars[0]:
            case c if c.isalpha():
                prev = read_dirs(prev, chars)
            case '(':
                prev = read_branches(prev, chars)
            case _:
                return prev
    return prev


def regex_to_path(regex: str) -> Node:
    chars = deque(regex.strip("^$"))
    start = Node()
    read_expr(set([start]), chars)
    return start


def print_node(node: Node, indent: int = 0) -> None:
    padding = '  ' * indent
    print(f'{padding} node {node.id} - {node.direction}')
    for child in node.next:
        print_node(child, indent + 1)


directions = {
    'W': (-1, 0),
    'E': (1, 0),
    'N': (0, -1),
    'S': (0, 1)
}

dir_to_door = {
    'W': '|',
    'E': '|',
    'N': '-',
    'S': '_'
}


def get_furthest_room(rooms: dict[Position, float]) -> tuple[Position, float]:
    furthest_room = max(
        (r for r in rooms if math.isfinite(rooms[r])), key=lambda r: rooms[r])
    return furthest_room, rooms[furthest_room]


def step(pos: Position, dir: Position) -> Position:
    x, y = pos
    dx, dy = dir
    return x + dx, y + dy


def traverse(start: Node) -> tuple[dict[Position, str], dict[Position, float]]:
    doors = dict[Position, str]()
    rooms = defaultdict[Position, float](lambda: math.inf)
    rooms[(0, 0)] = 0

    queue = deque([(start, (0, 0))])
    iterations = 0

    while queue:
        node, pos = queue.popleft()
        dist = rooms[pos]
        iterations += 1

        for child in node.next:
            dir = directions[child.direction]
            door = step(pos, dir)
            room = step(door, dir)
            doors[door] = dir_to_door[child.direction]
            if rooms[room] > dist + 1:
                rooms[room] = dist + 1
                queue.append((child, room))

    return doors, rooms


def print_grid(doors: dict[Position, str], rooms: set[Position]):
    xs = list(map(fst, doors)) + list(map(fst, rooms))
    ys = list(map(snd, doors)) + list(map(snd, rooms))
    min_x, max_x = min(xs) - 1, max(xs) + 1
    min_y, max_y = min(ys) - 1, max(ys) + 1

    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x + 1):
            if (x, y) == (0, 0):
                print('X', end='')
            elif (x, y) in rooms:
                print('.', end='')
            elif (x, y) in doors:
                print(doors[(x, y)], end='')
            else:
                print('#', end='')
        print()


def find_height(node: Node) -> int:
    return 1 + (max(map(find_height, node.next)) if node.next else 0)


def solve(lines: list[str]):
    start = regex_to_path(lines[0])
    return traverse(start)


def part1(doors: dict[Position, str], rooms: dict[Position, float]):
    _, dist = get_furthest_room(rooms)
    return dist


def part2(doors: dict[Position, str], rooms: dict[Position, float]):
    return len([1 for r in rooms if rooms[r] >= 1000])


run(part1, part2, process_input=solve)
