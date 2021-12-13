from enum import Enum
from typing import TypeAlias
from core import replace_at, run


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

    def __hash__(self) -> int:
        return self.id


Crash: TypeAlias = tuple[Cart, Cart]


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


def fmt_location(location: Location) -> str:
    return f'{location[0]},{location[1]}'


def solve(lines: list[str]) -> tuple[Location, Location]:
    track, carts = parse_input(lines)
    remaining_carts = set(carts)
    carts_by_location = dict(map(lambda cart: (cart.location, cart), carts))
    first_crash_location = None
    while len(remaining_carts) > 1:
        for cart in sorted(remaining_carts, key=lambda cart: (cart.location[1], cart.location[0])):
            if cart not in remaining_carts:
                continue

            del carts_by_location[cart.location]
            move(track, cart)

            if cart.location in carts_by_location:
                if first_crash_location is None:  # part1
                    first_crash_location = cart.location
                remaining_carts.remove(cart)
                remaining_carts.remove(carts_by_location[cart.location])
                del carts_by_location[cart.location]
            else:
                carts_by_location[cart.location] = cart
    last_cart = remaining_carts.pop()
    return first_crash_location, last_cart.location


run(part1=lambda p1, _: fmt_location(p1), part2=lambda _,
    p2: fmt_location(p2), process_input=solve)
