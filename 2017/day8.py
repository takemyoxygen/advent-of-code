from typing import NamedTuple, Union, List
from collections import namedtuple, defaultdict
import operator


Register = str
Literal = int
Operator = str
Operand = Union[Register, Literal]
Modification = str
Registers = dict[Register, Literal]

Condition = NamedTuple("Condition", [("left", Operand), ("operator", Operator), ("right", Operand)])
Instruction = NamedTuple("Instruction", [('register', Register), ("modification", Modification), ("value", Literal), ("condition", Condition)])


operators = dict([
    ('==', operator.eq),
    ('>', operator.gt),
    ('<', operator.lt),
    ('>=', operator.ge),
    ('<=', operator.le),
    ('!=', operator.ne)
])

def parse_input(lines: List[str]) -> List[Instruction]:
    def parse_operand(operand: str) -> Operand:
        return operand if operand.isalpha() else int(operand)

    def parse_instruction(line: str) -> Instruction:
        [reg, mod, val, _, left, op, right] = line.split(' ')
        condition = Condition(parse_operand(left), op, parse_operand(right))
        return Instruction(reg, mod, int(val), condition)

    return list(map(parse_instruction, lines))


def eval_condition(cond: Condition, registers: Registers) -> bool:
    def eval_operand(operand: Operand) -> Literal:
        return operand if isinstance(operand, int) else registers.get(operand, 0)

    def eval_operator(operator: Operator):
        op = operators[operator]
        if not op:
            raise Exception("Unsupported operator", operator)
        return op

    left = eval_operand(cond.left)
    right = eval_operand(cond.right)
    op = eval_operator(cond.operator)
    return op(left, right)


def eval_instructions(instructions: List[Instruction]) -> tuple[Registers, int]:
    registers: Registers = defaultdict(lambda: 0)
    highest = 0

    def apply_instruction(instruction: Instruction):
        value = instruction.value if instruction.modification == 'inc' else -instruction.value
        new_val = registers[instruction.register] + value
        registers[instruction.register] = new_val
        return new_val

    for instruction in instructions:
        cond = eval_condition(instruction.condition, registers)
        if cond:
            new_val = apply_instruction(instruction)
            highest = max(highest, new_val)

    return registers, highest


lines = open("input/day8.txt").readlines()
instructions = parse_input(lines)

registers, highest = eval_instructions(instructions)
max_val = max(registers.values())

print("Day1", max_val)
print("Day2", highest)