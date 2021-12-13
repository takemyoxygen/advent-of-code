from enum import Enum
from os import DirEntry
from typing import TypeAlias
from core import replace_at, run
from itertools import count

Track: TypeAlias = list[str]
Location: TypeAlias = tuple[int, int]


class Direction(Enum):
    LEFT = 'left',
    UP = 'up',
    RIGHT = 'right',
    DOWN = 'down'


directions_clockwise = list(Direction)


steps = {
    Direction.LEFT: (-1, 0),
    Direction.RIGHT: (1, 0),
    Direction.UP: (0, -1),
    Direction.DOWN: (0, 1)
}

turns = {
    '/': {
        Direction.UP: Direction.RIGHT,
        Direction.RIGHT: Direction.UP,
        Direction.LEFT: Direction.DOWN,
        Direction.DOWN: Direction.LEFT
    },
    '\\': {
        Direction.UP: Direction.LEFT,
        Direction.LEFT: Direction.UP,
        Direction.RIGHT: Direction.DOWN,
        Direction.DOWN: Direction.RIGHT
    }
}


class Cart:
    id: int
    location: tuple[int, int]
    direction: Direction
    intersections: int

    def __init__(self, id: str, location: tuple[int, int], direction: Direction) -> None:
        self.id = id
        self.location = location
        self.direction = direction
        self.intersections = 0

    def __str__(self) -> str:
        return f'Cart {self.id} at {self.location} moving {self.direction}'

    def __repr__(self) -> str:
        return self.__str__()


def parse_input(lines: list[str]) -> tuple[Track, list[Cart]]:
    track = []
    carts = []
    id = 1
    for i in range(len(lines)):
        track.append(lines[i])
        for ch in range(len(lines[i])):
            match track[i][ch]:
                case '>':
                    track[i] = replace_at(track[i], ch, '-')
                    carts.append(Cart(id, (ch, i), Direction.RIGHT))
                    id += 1
                case '<':
                    track[i] = replace_at(track[i], ch, '-')
                    carts.append(Cart(id, (ch, i), Direction.LEFT))
                    id += 1
                case '^':
                    track[i] = replace_at(track[i], ch, '|')
                    carts.append(Cart(id, (ch, i), Direction.UP))
                    id += 1
                case 'v':
                    track[i] = replace_at(track[i], ch, '|')
                    carts.append(Cart(id, (ch, i), Direction.DOWN))
                    id += 1
    return track, carts


def next_location(location: Location, dir: Direction) -> Location:
    x, y = location
    dx, dy = steps[dir]
    return x + dx, y + dy


def get_dir_after_intersection(cart: Cart) -> Direction:
    match cart.intersections % 3:
        case 0:
            return directions_clockwise[(directions_clockwise.index(cart.direction) - 1) % len(directions_clockwise)]
        case 1:
            return cart.direction
        case 2:
            return directions_clockwise[(directions_clockwise.index(cart.direction) + 1) % len(directions_clockwise)]


def move(track: Track, cart: Cart) -> None:
    match track[cart.location[1]][cart.location[0]]:
        case '/' | '\\' as char:
            cart.direction = turns[char][cart.direction]
        case '+':
            cart.direction = get_dir_after_intersection(cart)
            cart.intersections += 1
    cart.location = next_location(cart.location, cart.direction)


def print_track(track: Track, carts: list[Cart]) -> None:
    carts_by_position = dict(map(lambda cart: (cart.location, cart), carts))
    for y in range(len(track)):
        for x in range(len(track[y])):
            if (x, y) in carts_by_position:
                match carts_by_position[x, y].direction:
                    case Direction.LEFT: print('<', end='')
                    case Direction.RIGHT: print('>', end='')
                    case Direction.UP: print('^', end='')
                    case Direction.DOWN: print('v', end='')
            else:
                print(track[y][x], end='')
        print('')


def part1(track: Track, carts: list[Cart]):
    for time in count(1):
        print('Processing time', time)
        cart_locations = set()
        for cart in sorted(carts, key=lambda cart: (cart.location[1], cart.location[0])):
            move(track, cart)
            if cart.location in cart_locations:
                return cart.location
            else:
                cart_locations.add(cart.location)
        # print_track(track, carts)


run(part1, process_input=parse_input)
