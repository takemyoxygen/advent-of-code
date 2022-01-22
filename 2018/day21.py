from typing import TypeAlias, Callable
import operator
from core import run

Registers: TypeAlias = list[int]
Operation: TypeAlias = Callable[[Registers, int, int, int], None]
Instruction: TypeAlias = tuple[str, int, int, int]


def register(registers: Registers, x: int) -> int:
    return registers[x]


def immediate(registers: Registers, x: int) -> int:
    return x


def op(get_a: Callable[[Registers, int], int], get_b: Callable[[Registers, int], int], get_result: Callable[[int, int], int]) -> Operation:
    def perform(registers: Registers, a: int, b: int, c: int) -> None:
        a_val = get_a(registers, a)
        b_val = get_b(registers, b)
        registers[c] = get_result(a_val, b_val)
    return perform


def gt(a: int, b: int) -> int:
    return 1 if a > b else 0


def eq(a: int, b: int) -> int:
    return 1 if a == b else 0


operations: dict[str, Operation] = {
    "addr": op(register, register, operator.add),
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


def parse(lines: list[str]) -> tuple[int, list[Instruction]]:
    inst_pointer_line, *instruction_lines = lines
    instr_pointer = int(inst_pointer_line.split()[1])
    instructions = [(instr, int(a), int(b), int(c)) for [instr, a, b, c] in (
        line.split() for line in instruction_lines)]
    return instr_pointer, instructions


def part1(instruction_register: int, instructions: list[Instruction]):
    instruction_pointer = 0
    registers = [0] * 6
    while 0 <= instruction_pointer < len(instructions):
        registers[instruction_register] = instruction_pointer
        operation, a, b, c = instructions[instruction_pointer]
        operations[operation](registers, a, b, c)
        instruction_pointer = registers[instruction_register]
        instruction_pointer += 1
        if instruction_pointer == 28:
            return registers[5]


def part2(instruction_register: int, instructions: list[Instruction]):
    r3, r5 = 0, 0
    prev = None
    seen_r5 = set[int]()

    def inner(r3: int, r5: int) -> int:
        while True:
            r4 = r3 & 255
            r5 += r4
            r5 = r5 & 16777215
            r5 = r5 * 65899
            r5 = r5 & 16777215

            if r3 < 256:
                return r5

            r3 = r3 // 256

    while True:
        r3 = r5 | 65536
        r5 = 521363

        r5 = inner(r3, r5)

        if r5 in seen_r5:
            return prev
        else:
            prev = r5
            seen_r5.add(r5)


run(part1, part2, process_input=parse)
