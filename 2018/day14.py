import operator
import functools

def part1(after: int) -> str:
    recipes = [3, 7]
    e1, e2 = 0, 1
    while len(recipes) < after + 10:
        r1, r2 = divmod(recipes[e1] + recipes[e2], 10)
        if r1 != 0:
            recipes.append(r1)
        recipes.append(r2)
        e1 = (e1 + 1 + recipes[e1]) % len(recipes)
        e2 = (e2 + 1 + recipes[e2]) % len(recipes)
    return "".join(map(str, recipes[after:]))



def ends_with(xs: list[int], end: list[int]) -> bool:
  if (len(xs) < len(end)):
    return False

  return all(map(operator.eq, xs[-len(end):], end))


def ends_with_str(s1: str, end: str) -> bool:
  return s1.find(end, len(s1) - len(end)) >= 0


@functools.cache
def digit(s: str) -> int:
  return int(s)



def part2(input: str) -> str:
    recipes = "37"
    e1, e2 = 0, 1
    while not ends_with_str(recipes, input):
    # for _ in range(0, 60):
        recipes += str(digit(recipes[e1]) + digit(recipes[e2]))
        e1 = (e1 + 1 + digit(recipes[e1])) % len(recipes)
        e2 = (e2 + 1 + digit(recipes[e2])) % len(recipes)

        # print(recipes, '->', e1, e2)

        if len(recipes) % 1000000 == 0:
          print('Recipes: ', len(recipes))
  
    return len(recipes) - len(input)


print(part2("880751"))
