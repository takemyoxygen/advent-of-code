from functools import reduce
import core
import itertools


def parse_input(lines: list[str]) -> tuple[set[int], set[str]]:
    [init_state_str, empty, *instructions] = lines
    init_state_str = init_state_str.replace("initial state: ", "")
    init_state = set(i for i in range(0, len(init_state_str))
                     if init_state_str[i] == "#")
    plant_instructions = set(pattern for [pattern, result] in (
        instr.split(" => ") for instr in instructions) if result == "#")
    return init_state, plant_instructions


def plants_to_str(plants: set[int]) -> str:
    return "".join("#" if i in plants else "." for i in range(min(plants), max(plants) + 1))


def get_next_plants(plants: set[int], plant_instructions: set[str]) -> set[int]:
    next_plants = set()
    for pos in range(min(plants) - 3, max(plants) + 4):
        key = "".join('#' if idx in plants else "." for idx in (
            pos + i - 2 for i in range(0, 5)))
        if key in plant_instructions:
            next_plants.add(pos)
    return next_plants


def part1(plants: set[int], plant_instructions: set[str]):
    current_plants = plants
    for _ in range(20):
        current_plants = get_next_plants(current_plants, plant_instructions)
    return sum(current_plants)


def part2(plants: set[int], plant_instructions: set[str]):
    target_gen = 50000000000
    current_plants = plants
    current_plants_str = plants_to_str(current_plants)
    for gen in itertools.count(1):
        next_plants = get_next_plants(current_plants, plant_instructions)
        next_plants_str = plants_to_str(next_plants)
        if next_plants_str == current_plants_str:
            remaining_gens = target_gen - gen
            return sum(next_plants) + remaining_gens * len(next_plants)
        else:
            current_plants = next_plants
            current_plants_str = next_plants_str


core.run(part1, part2, process_input=parse_input)
