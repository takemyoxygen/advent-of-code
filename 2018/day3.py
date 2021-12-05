import re
from typing import NamedTuple
import core


class Claim(NamedTuple):
    id: int
    x: int
    y: int
    width: int
    height: int


def parse_input(lines: list[str]) -> list[Claim]:
    pattern = "^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"
    return [
        Claim(int(id), int(x), int(y), int(w), int(h))
        for id, x, y, w, h in (re.match(pattern, line).groups() for line in lines)
    ]


def solve(lines: list[str]):
    claims = parse_input(lines)
    max_x = max(map(lambda claim: claim.x + claim.width, claims))
    max_y = max(map(lambda claim: claim.y + claim.height, claims))

    grid = []
    for i in range(0, max_y):
        grid.append([0] * max_x)

    shared_squares = set()
    intact_claims = set(map(lambda claim: claim.id, claims))

    for claim in claims:
        for x in range(claim.x, claim.x + claim.width):
            for y in range(claim.y, claim.y + claim.height):
                if grid[x][y] != 0:
                    shared_squares.add((x, y))
                    intact_claims.discard(grid[x][y])
                    intact_claims.discard(claim.id)
                else:
                    grid[x][y] = claim.id

    return shared_squares, intact_claims


def part1(claims: Claim):
    max_x = max(map(lambda claim: claim.x + claim.width, claims))
    max_y = max(map(lambda claim: claim.y + claim.height, claims))

    grid = []
    for i in range(0, max_y):
        grid.append([0] * max_x)

    shared_squares = set()

    for claim in claims:
        for x in range(claim.x, claim.x + claim.width):
            for y in range(claim.y, claim.y + claim.height):
                if grid[x][y] != 0:
                    shared_squares.add((x, y))
                else:
                    grid[x][y] = claim.id

    return len(shared_squares)


def part2(claims: Claim):
    max_x = max(map(lambda claim: claim.x + claim.width, claims))
    max_y = max(map(lambda claim: claim.y + claim.height, claims))

    grid = []
    for i in range(0, max_y):
        grid.append([0] * max_x)

    shared_squares = set()
    intact_claims = set(map(lambda claim: claim.id, claims))

    for claim in claims:
        for x in range(claim.x, claim.x + claim.width):
            for y in range(claim.y, claim.y + claim.height):
                if grid[x][y] != 0:
                    shared_squares.add((x, y))
                    intact_claims.discard(grid[x][y])
                    intact_claims.discard(claim.id)
                else:
                    grid[x][y] = claim.id

    return intact_claims

core.run(part1=lambda shared, intact: len(shared), part2=lambda shared,intact: intact.pop(), process_input=solve)