from core import run


def step(recipes: list[int], e1: int, e2: int) -> tuple[int, int]:
    r1, r2 = divmod(recipes[e1] + recipes[e2], 10)
    if r1 != 0:
        recipes.append(r1)
    recipes.append(r2)
    e1 = (e1 + 1 + recipes[e1]) % len(recipes)
    e2 = (e2 + 1 + recipes[e2]) % len(recipes)
    return e1, e2


def part1(after: str) -> str:
    after = int(after)
    recipes = [3, 7]
    e1, e2 = 0, 1
    while len(recipes) < after + 10:
        e1, e2 = step(recipes, e1, e2)
    return "".join(map(str, recipes[after:]))


def contains(xs: list[int], start: int, another: list[int]):
    for s in range(start, len(xs) - len(another) + 1):
        if(all(map(lambda i: xs[s + i] == another[i], range(len(another))))):
            return s
    return -1


def part2(input: str) -> str:
    required_recipes = list(map(int, input))
    recipes = [3, 7]
    e1, e2 = 0, 1
    while True:
        e1, e2 = step(recipes, e1, e2)
        input_index = contains(recipes, len(recipes) -
                               len(required_recipes) - 1, required_recipes)
        if input_index >= 0:
            return input_index


run(part1, part2, input="880751")
