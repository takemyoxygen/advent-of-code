import re
from typing import Callable, NamedTuple, TypeAlias
from core import run
import operator


Registers: TypeAlias = list[int]
Operation: TypeAlias = Callable[[Registers, int, int, int], Registers]
Instruction: TypeAlias = list[int]


class Sample(NamedTuple):
    before: Registers
    instruction: Instruction
    after: Registers


def extract_numbers(s: str) -> list[int]:
    return list(map(int, re.findall(r'\d+', s)))


def parse(input: list[str]) -> tuple[list[Sample], list[Instruction]]:
    rest: list[str] = input
    samples: list[Sample] = []
    end_of_samples = False
    while not end_of_samples:
        before, instruction, after, _, *rest = rest
        if before.startswith("Before"):
            samples.append(Sample(extract_numbers(before), extract_numbers(
                instruction), extract_numbers(after)))
        else:
            rest = [after] + rest
            end_of_samples = True

    instructions = list(map(extract_numbers, rest))
    return samples, instructions


def register(registers: Registers, x: int) -> int:
    return registers[x]


def immediate(registers: Registers, x: int) -> int:
    return x


def op(get_a: Callable[[Registers, int], int], get_b: Callable[[Registers, int], int], get_result: Callable[[int, int], int]) -> Operation:
    def perform(registers: Registers, a: int, b: int, c: int) -> Registers:
        new_registers = registers.copy()
        a_val = get_a(registers, a)
        b_val = get_b(registers, b)
        new_registers[c] = get_result(a_val, b_val)
        return new_registers
    return perform


def gt(a: int, b: int) -> int:
    return 1 if a > b else 0


def eq(a: int, b: int) -> int:
    return 1 if a == b else 0


operations: dict[str, Operation] = {
    "adddr": op(register, register, operator.add),
    "addi": op(register, immediate, operator.add),
    "mulr": op(register, register, operator.mul),
    "muli": op(register, immediate, operator.mul),
    "banr": op(register, register, operator.and_),
    "bani": op(register, immediate, operator.and_),
    "borr": op(register, register, operator.or_),
    "bori": op(register, immediate, operator.or_),
    "setr": op(register, immediate, lambda a, _: a),
    "seti": op(immediate, immediate, lambda a, _: a),
    "gtir": op(immediate, register, gt),
    "gtri": op(register, immediate, gt),
    "gtrr": op(register, register, gt),
    "eqir": op(immediate, register, eq),
    "eqri": op(register, immediate, eq),
    "eqrr": op(register, register, eq),
}


def operation_matches(sample: Sample, operation: str) -> bool:
    [_, a, b, c] = sample.instruction
    registers = sample.before.copy()
    result = operations[operation](registers, a, b, c)
    return result == sample.after


def find_operations_for(sample: Sample) -> list[str]:
    return list(filter(lambda o: operation_matches(sample, o), operations))


def get_opcode_mappings(samples: list[Sample]) -> list[str]:
    mapping_options: dict[int, set[str]] = dict()
    for sample in samples:
        operations = find_operations_for(sample)
        opcode = sample.instruction[0]
        if opcode in mapping_options:
            mapping_options[opcode].intersection_update(operations)
        else:
            mapping_options[opcode] = set(operations)

    results: list[str] = [''] * 16
    while mapping_options:
        unambiguous_opcode = next(
            opcode for opcode in mapping_options if len(mapping_options[opcode]) == 1)
        operation = mapping_options[unambiguous_opcode].pop()
        results[unambiguous_opcode] = operation
        del mapping_options[unambiguous_opcode]
        for mapping in mapping_options.values():
            mapping.discard(operation)

    return results


def part1(samples: list[Sample], instructions: list[Instruction]) -> int:
    return len([s for s in samples if len(find_operations_for(s)) >= 3])


def part2(samples: list[Sample], instructions: list[Instruction]) -> int:
    opcode_mappings = get_opcode_mappings(samples)
    registers = [0] * 4
    for opcode, a, b, c in instructions:
        registers = operations[opcode_mappings[opcode]](registers, a, b, c)
    return registers[0]


run(part1, part2, process_input=parse)
