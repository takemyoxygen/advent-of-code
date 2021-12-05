from typing import Union, Iterable


def part1(changes: list[str]) -> int:
  return sum(changes)


def repeat_list(xs: list[int]) -> Iterable[int]:
  while True:
    for x in xs:
      yield x


def part2(changes: list[str]) -> Union[int, None]:
  frequencies = set([0])
  frequency = 0
  for change in repeat_list(changes):
    frequency += change
    if frequency in frequencies:
      return frequency
    frequencies.add(frequency)
  return None

changes = [int(line) for line in open('input/day1.txt').readlines()]
print('Part 1', part1(changes))
print('Part 2', part2(changes))