from typing import Callable, List

def contains_uniq_words(line: str, transform_word: Callable[[str], str]) -> bool:
    seen = set()
    for word in line.strip().split(" "):
      transformed = transform_word(word)
      if transformed in seen:
        return False
      seen.add(transformed)
    return True

def lines_with_uniq_words(lines: List[str], transform_word: Callable[[str], str]) -> int:
  return len(list(filter(lambda line: contains_uniq_words(line, transform_word), lines)))


def day1(lines: List[str]) -> None:
  return lines_with_uniq_words(lines, lambda word: word)

def day2(lines: List[str]) -> None:
  def encode_word(word: str) -> str:
    char_counts = {}
    for i in range(len(word)):
      char_counts[word[i]] = char_counts.get(word[i], 0) + 1
    return "".join([char + str(char_counts[char]) for char in sorted(char_counts.keys())])

  return lines_with_uniq_words(lines, encode_word)


f = open("input/day4.txt", "r")
lines = f.readlines()

print('Day1', day1(lines))
print('Day2', day2(lines))


