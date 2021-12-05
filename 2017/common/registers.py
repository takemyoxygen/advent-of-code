from typing import Callable, Union
import operator

Operand = Union[str, int]
Register = str
Position = int
Registers = dict[Register, int]
Modification = Callable[[Registers, Position, Operand, Operand], Position]


def modify_register(modification: Callable[[int, int], int]) -> Modification:
    def apply_modification(registers: Registers, position: Position, register: str, value: Operand) -> Position:
        registers[register] = modification(registers[register], eval_operand(value, registers))
        return position + 1

    return apply_modification


def eval_operand(operand: Operand, registers: Registers) -> int:
    try:
        return int(operand)
    except ValueError:
        return registers[operand]


def perform_jump(registers: Registers, position: Position, condition: Operand, offset: Operand, predicate: Callable[[int], bool]):
    return position + (eval_operand(offset, registers) if predicate(eval_operand(condition, registers)) else 1)


def perform_jgz(registers: Registers, position: Position, condition: Operand, offset: Operand):
    return perform_jump(registers, position, condition, offset, lambda c: c > 0)


def perform_jnz(registers: Registers, position: Position, condition: Operand, offset: Operand):
    return perform_jump(registers, position, condition, offset, lambda c: c != 0)


handlers = {
    'set': modify_register(lambda old, new: new),
    'add': modify_register(operator.add),
    'mul': modify_register(operator.mul),
    'mod': modify_register(operator.mod),
    'sub': modify_register(operator.sub),
    'jgz': perform_jgz,
    'jnz': perform_jnz
}