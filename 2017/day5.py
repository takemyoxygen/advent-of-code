from typing import Callable, List

def jump(offsets: List[int], next_offset: Callable[[int], int]) -> int:
    steps = 0
    pos = 0
    while pos >= 0 and pos < len(offsets):
      next_pos = pos + offsets[pos]
      offsets[pos] = next_offset(offsets[pos])
      steps += 1
      pos = next_pos
    return steps

def day1(offsets: List[int]) -> int:
  return jump(offsets, lambda x: x + 1)

def day2(offsets: List[int]) -> int:
  return jump(offsets, lambda x: x - 1 if x >= 3 else x + 1)

input = list(map(int, open("input/day5.txt").readlines()))
print('Day1', day1(input.copy()))
print('Day2', day2(input.copy()))
