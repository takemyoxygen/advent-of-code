from functools import reduce
from typing import Union
import core

def process_word(word: str) -> tuple[int, int]:
  char_counts = {}
  for char in word:
    char_counts[char] = char_counts.get(char, 0) + 1

  hasN = lambda n: any(map(lambda char: char_counts[char] == n, char_counts))

  return 1 if hasN(2) else 0, 1 if hasN(3) else 0


def sum_tuple(t1: tuple[int, int], t2: tuple[int, int]) -> tuple[int, int]:
  return t1[0] + t2[0], t1[1] + t2[1]


def part1(words: list[str]) -> int:
  has2, has3 = reduce(sum_tuple, map(process_word, words), (0, 0))
  return has2 * has3


def find_single_diff_index(s1: str, s2: str) -> Union[int, None]:
  single_diff_index = None

  for i in range(0, len(s1)):
    if s1[i] != s2[i]:
      if single_diff_index is None:
        single_diff_index = i
      else:
        return None

  return single_diff_index


def find_matching_boxes(ids: list[str]) -> tuple[str, str, int]:
  for i in range(0, len(ids) - 1):
    for j in range(i, len(ids)):
      diff_index = find_single_diff_index(ids[i], ids[j])
      if diff_index is not None:
        return ids[i], ids[j], diff_index
  raise Exception('Unable to find boxes with matching IDs')


def part2(words: list[str]) -> str:
  b1, b2, index = find_matching_boxes(words)
  common = b1[0:index] + b1[index + 1:]
  return common



core.run(part1, part2)