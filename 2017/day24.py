from typing import List, Union
from collections import defaultdict

Component = tuple[int, int]


def parse_input(lines: list[str]) -> list[Component]:
  return [tuple(map(int, line.split('/'))) for line in lines]


def build_components_lookup(components: List[Component]) -> dict[int, List[Component]]:
  lookup = defaultdict(list)
  for component in components:
    x, y = component
    lookup[x].append(component)
    lookup[y].append(component)
  return lookup


def get_bridges(components: List[Component]) -> List[tuple[int, int]]: # length * strength list
  lookup = build_components_lookup(components)
  used = set()
  bridges = []

  def loop(start: int, total: int, length: int) -> None:
    matching = list(filter(lambda c: c not in used, lookup[start]))

    if len(matching) == 0:
      bridges.append((length, total))
      return

    for c in matching:
      used.add(c)
      another_end = c[1] if start == c[0] else c[0]
      loop(another_end, total + sum(c), length + 1)
      used.remove(c)

  loop(0, 0, 0)

  return bridges


lines = [line.rstrip('\n') for line in open('input/day24.txt').readlines()]
components = parse_input(lines)
bridges = get_bridges(components)

print('Part 1', max(map(lambda c: c[1], bridges)))
print('Part 2', sorted(bridges, reverse=True)[0][1])