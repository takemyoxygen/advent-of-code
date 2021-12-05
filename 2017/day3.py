from typing import List

directions_and_offsets = [
  ((1, 0), 1),
  ((0, -1), 0),
  ((-1, 0), 1),
  ((0, 1), 0)
]

def next(pos: tuple[int, int], dir: tuple[int, int]) -> tuple[int, int]:
  x, y = pos
  dx, dy = dir
  return x + dx, y + dy

def adjacent_to(pos: tuple[int, int]) -> List[tuple[int, int]]:
  x, y = pos
  return [
    (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1),
    (x + 1, y + 1),
    (x - 1, y - 1),
    (x + 1, y - 1),
    (x - 1, y + 1)
  ]

def day2(input: int) -> int:
  grid = {}
  pos = 0, 0
  offset = 0
  val = 1
  direction_index = 0
  grid[pos] = val
  while val < input:
    direction, extra_offset = directions_and_offsets[direction_index]
    offset += extra_offset
    steps = 0
    while steps < offset and val < input:
      pos = next(pos, direction)
      val = sum(map(lambda adj: grid.get(adj, 0), adjacent_to(pos)))
      grid[pos] = val
      steps += 1
    direction_index = (direction_index + 1) % len(directions_and_offsets)
  return grid[pos]

print(day2(368078))

