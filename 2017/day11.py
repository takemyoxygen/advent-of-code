from typing import List
from itertools import accumulate
from math import ceil


directions = dict([
  ('n', (1, 0)),
  ('s', (-1, 0)),
  ('ne', (0.5, 1)),
  ('nw', (0.5, -1)),
  ('se', (-0.5, 1)),
  ('sw', (-0.5, -1)),
])

def sum_point(p1: tuple[int, int], p2: tuple[int, int]) -> tuple[int, int]:
  return p1[0] + p2[0], p1[1] + p2[1]

def coord_to_dist(c: tuple[int, int]) -> int:
  [vertical, horizontal] = [abs(x) for x in c]
  return int(vertical + horizontal / 2)


def get_distances(steps: List[str]) -> List[int]:
  coords = accumulate(steps, func=lambda pos, step: sum_point(pos, directions[step]), initial=(0, 0))
  return [coord_to_dist(coord) for coord in coords]


# input = 'ne,ne,ne'
input = open('input/day11.txt').read()
distances = get_distances(input.split(','))
print('Part 1', distances[-1])
print('Part 2', max(distances))

