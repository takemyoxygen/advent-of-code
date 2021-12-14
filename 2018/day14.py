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

part1(9)