from enum import Enum
from typing import Callable, TypeAlias, Any
from collections.abc import Iterable, Iterator
from collections import deque
from core import replace_at, run
from itertools import takewhile, count


class UnitType(Enum):
    Elf = 'E'
    Goblin = 'G'

    @staticmethod
    def parse(s: str) -> "UnitType":
        match s:
            case 'E': return UnitType.Elf
            case 'G': return UnitType.Goblin
            case _: raise Exception(f'Invalid unit type "{s}"')


Location: TypeAlias = tuple[int, int]


class Unit:
    type: UnitType
    health: int
    power: int
    location: Location

    def __init__(self, type: UnitType, location: Location) -> None:
        self.type = type
        self.health = 200
        self.power = 3
        self.location = location

    @property
    def alive(self) -> bool:
        return self.health > 0

    def __repr__(self) -> str:
        return f"{self.type.value} at {self.location}"

    def copy(self) -> "Unit":
        return Unit(self.type, self.location)


def parse(input: list[str]) -> tuple[list[str], list[Unit]]:
    units: list[Unit] = []
    unit_chars = ['E', 'G']
    combat_map: list[str] = []

    for i in range(len(input)):
        combat_map.append(input[i])
        for j in range(len(combat_map[i])):
            if combat_map[i][j] in unit_chars:
                units.append(Unit(UnitType.parse(combat_map[i][j]), (i, j)))
                combat_map[i] = replace_at(combat_map[i], j, '.')

    return combat_map, units


def print_state(combat_map: list[str], units: list[Unit]):
    units_by_location = dict(map(lambda unit: (unit.location, unit), units))
    for i in range(len(combat_map)):
        for j in range(len(combat_map[i])):
            match units_by_location.get((i, j), None):
                case None: print(combat_map[i][j], end='')
                case unit: print(unit.type.value, end='')
        print()


steps: list[Location] = [(-1, 0), (0, -1), (0, 1), (1, 0)]


def enemies_of(unit: Unit, all_units: list[Unit]) -> list[Unit]:
    return [u for u in all_units if u.type != unit.type and u.alive]


def adjacent_to(location: Location) -> Iterable[Location]:
    i, j = location
    return ((i + di, j + dj) for (di, dj) in steps)


def is_free_location(combat_map: list[str], units: Iterable[Unit]) -> Callable[[Location], bool]:
    unit_locations = set(map(lambda unit: unit.location, units))
    return lambda loc: combat_map[loc[0]][loc[1]] == '.' and loc not in unit_locations


def find_reachable(unit: Unit, all_units: list[Unit], combat_map: list[str], locations: Iterable[Location]) -> Iterator[tuple[Location, int]]:
    _, distances = bfs(unit.location, locations,
                       is_free_location(combat_map, all_units))
    return distances


def find_nearest(reachable: Iterator[tuple[Location, int]]) -> list[Location]:
    try:
        loc, shortest_dist = next(reachable)
        return [loc] + [l for (l, _) in takewhile(lambda x: x[1] == shortest_dist, reachable)]
    except StopIteration:
        return []


def choose_move_location(locations: list[Location]) -> Location | None:
    if not locations:
        return None
    return sorted(locations)[0]


def bfs(start: Location, destinations: Iterable[Location], can_visit: Callable[[Location], bool]) -> tuple[dict[Location, int], Iterator[tuple[Location, int]]]:
    dest_set = set(destinations)
    queue: deque[tuple[Location, int]] = deque([(start, 0)])
    distances: dict[Location, int] = {}

    def iter() -> Iterator[tuple[Location, int]]:
        while queue:
            loc, dist = queue.popleft()
            if loc not in distances:
                distances[loc] = dist
                available_adjacent = filter(lambda l: can_visit(
                    l) and l not in distances, adjacent_to(loc))
                for adj in available_adjacent:
                    if adj not in distances:
                        queue.append((adj, dist + 1))
                if loc in dest_set:
                    yield loc, dist
    return distances, iter()


def get_next_location(start: Location, destination: Location, combat_map: list[str], all_units: list[Unit]) -> Location:
    free_loc = is_free_location(combat_map, all_units)
    distances, iter = bfs(
        destination, [start], lambda loc: free_loc(loc) or loc == start)
    _, dist = next(iter)
    return next(loc for loc in adjacent_to(start) if distances.get(loc, -1) == dist - 1)


def attack(unit: Unit, enemies: list[Unit], on_death: Callable[[Unit], None] | None = None) -> None:
    enemies_by_location = dict(map(lambda u: (u.location, u), enemies))
    try:
        adjacent_enemies = (enemies_by_location[loc] for loc in adjacent_to(
            unit.location) if loc in enemies_by_location)
        target = sorted(adjacent_enemies, key=lambda enemy: (
            enemy.health, enemy.location))[0]
        target.health -= unit.power
        if not target.alive and on_death is not None:
            on_death(target)
    except IndexError:
        return


def move(unit: Unit, combat_map: list[str], units: list[Unit], enemies: list[Unit]) -> None:
    alive_units = [u for u in units if u.alive]
    attack_locations = (
        loc for e in enemies for loc in adjacent_to(e.location))
    reachable_locations = find_reachable(
        unit, alive_units, combat_map, attack_locations)
    nearest = find_nearest(reachable_locations)
    chosen = choose_move_location(nearest)
    if chosen is not None and chosen != unit.location:
        next_location = get_next_location(
            unit.location, chosen, combat_map, alive_units)
        unit.location = next_location


def turn(combat_map: list[str], units: list[Unit], on_death: Callable[[Unit], None] | None = None) -> bool:
    for unit in sorted(units, key=lambda unit: unit.location):
        if unit.alive:
            enemies = enemies_of(unit, units)
            if not enemies:
                return False
            move(unit, combat_map, units, enemies)
            attack(unit, enemies, on_death)
    return True


def game(combat_map: list[str], units: list[Unit], stop_on_elf_death: bool = False) -> int:
    rounds = 0

    while turn(combat_map, units):
        if stop_on_elf_death and any(u for u in units if u.type == UnitType.Elf and not u.alive):
            break
        units = [u for u in units if u.alive]
        rounds += 1
    return rounds


def copy_units(units: list[Unit]) -> list[Unit]:
    return [unit.copy() for unit in units]


def part1(combat_map: list[str], units: list[Unit]) -> int:
    units_copy = copy_units(units)
    rounds = game(combat_map, units_copy)
    return rounds * sum(u.health for u in units_copy if u.alive)


def part2(combat_map: list[str], units: list[Unit]) -> Any:
    for elves_power in count(3):
        units_copy = copy_units(units)

        elves = [u for u in units_copy if u.type == UnitType.Elf]
        for unit in elves:
            if unit.type == UnitType.Elf:
                unit.power = elves_power
        rounds = game(combat_map, units_copy, True)
        alive = [u for u in units_copy if u.alive]
        elves_dead = len(elves) - \
            len([u for u in alive if u.type == UnitType.Elf])
        if elves_dead == 0:
            return rounds * sum(u.health for u in alive)


run(part1, part2, process_input=parse)
