from enum import Enum
import re
from core import fst, snd, next_id, run


class Type(Enum):
    IMMUNE = 'Immune'
    INFECTION = 'Infection'


class Group:
    __id: int
    type: Type
    units: int
    hitpoints: int
    damage: int
    damage_type: str
    initiative: int
    weak: set[str]
    immune: set[str]

    def __init__(self, type: Type, units: int, hitpoints: int, damage: int, damage_type: str, initiative: int, weak: list[str] = [], immune: list[str] = []) -> None:
        self.__id = next_id()
        self.type = type
        self.units = units
        self.hitpoints = hitpoints
        self.damage = damage
        self.damage_type = damage_type
        self.initiative = initiative
        self.weak = set(weak)
        self.immune = set(immune)

    def __hash__(self) -> int:
        return self.__id

    def __repr__(self) -> str:
        return f'{self.type.value} group {self.__id} with {self.units} units ({self.hitpoints} HP) and {self.damage} damage'

    def effective_power(self) -> int:
        return self.damage * self.units


def parse_traits(traits: str | None) -> tuple[list[str], list[str]]:
    weak, immune = [], []

    if traits is not None:
        for token in traits.strip('()').split('; '):
            if token.startswith('immune'):
                immune = token.replace('immune to ', '').split(', ')
            else:
                weak = token.replace('weak to ', '').split(', ')

    return weak, immune


def parse_group(type: Type, line: str) -> Group:
    units, hitpoints, traits, damage, damage_type, initiative = re.match(
        r'(\d+) units.*with (\d+).*hit points (\(.*\))?.*does (\d+) ([a-z]+) damage.*initiative (\d+)', line).groups()

    weak, immune = parse_traits(traits)

    return Group(type, int(units), int(hitpoints), int(damage), damage_type, int(initiative), weak, immune)


def parse(lines: list[str]) -> list[Group]:
    type = None
    groups = list[Group]()
    for line in lines:
        if line.startswith('Immune'):
            type = Type.IMMUNE
        elif line.startswith('Infection'):
            type = Type.INFECTION
        elif line:
            groups.append(parse_group(type, line))
    return groups


def effective_damage(attacker: Group, victim: Group) -> int:
    base_damage = attacker.effective_power()

    match attacker.damage_type:
        case x if x in victim.weak: return 2 * base_damage
        case x if x in victim.immune: return 0
        case _: return base_damage


def pick_targets(groups: list[Group]) -> list[tuple[Group, Group | None]]:
    non_targets = set(groups)
    result = list[tuple[Group, Group | None]]()

    for attacker in sorted(groups, key=lambda group: (-group.effective_power(), -group.initiative)):
        target, damage = max(((g, effective_damage(attacker, g)) for g in non_targets if g.type !=
                             attacker.type), key=lambda x: (x[1], x[0].effective_power(), x[0].initiative), default=(None, 0))
        if damage == 0:
            target = None

        if target is not None:
            non_targets.remove(target)

        result.append((attacker, target))

    return result


def deal_damage(pairs: list[tuple[Group, Group | None]]) -> tuple[list[Group], int]:
    total_killed = 0
    for attacker, target in sorted(pairs, key=lambda pair: pair[0].initiative, reverse=True):
        if attacker.units > 0 and target is not None:
            damage = effective_damage(attacker, target)
            killed = min(damage // target.hitpoints, target.units)
            target.units -= killed
            total_killed += killed

    return [g for (g, _) in pairs if g.units > 0], total_killed


def has_winner(groups: list[Group]) -> bool:
    return len(set(map(lambda g: g.type, groups))) == 1


def play(initial_groups: list[Group], boost: int = 0) -> tuple[Type | None, int]:
    groups = initial_groups

    for immune in (g for g in groups if g.type == Type.IMMUNE):
        immune.damage += boost

    while not has_winner(groups):
        pairs = pick_targets(groups)
        groups, killed = deal_damage(pairs)
        if killed == 0:
            return None, 0

    winner = groups[0].type
    units = sum(map(lambda g: g.units, groups))
    return winner, units


def part1(lines: list[str]) -> int:
    return snd(play(parse(lines)))


def search(lines: list[str], lo: int, hi: int) -> int:
    if lo > hi:
        return lo

    m = (lo + hi) // 2
    winner = fst(play(parse(lines), m))
    if winner != Type.IMMUNE:
        return search(lines, m + 1, hi)
    return search(lines, lo, m - 1)


def part2(lines: list[str]):
    boost = 0
    winner = Type.INFECTION

    while winner == Type.INFECTION:
        boost += 100
        winner = fst(play(parse(lines), boost))

    min_boost = search(lines, boost - 100, boost)
    return snd(play(parse(lines), min_boost))


run(part1, part2)
