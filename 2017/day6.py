from typing import List


def cycle(input: List[int]) -> int:
  block_index = max(range(len(input)), key=lambda i: input[i])
  blocks = input[block_index]
  input[block_index] = 0
  next = block_index
  for _ in range(blocks):
    next = (next + 1) % len(input)
    input[next] += 1


def solve(input: List[int]) -> int:
  seen = {}
  cycles = 0
  while tuple(input) not in seen:
    seen[tuple(input)] = cycles
    cycle(input)
    cycles += 1

  return cycles, cycles - seen[tuple(input)]


input_line = "2	8	8	5	4	2	3	1	5	5	1	2	15	13	5	14"
input = [int(x) for x in input_line.split("\t")]
cycles, loop_size = solve(input)
print("Day1:", cycles, "Day2:", loop_size)

