#!/usr/bin/env python3

import os
import argparse
from typing import Any, Callable, TypeVar
from collections.abc import Sequence
import time

A = TypeVar("A")

def fst(x: Sequence[A]) -> A:
    return x[0]


def snd(x: Sequence[A]) -> A:
    return x[1]


def replace_at(s: str, i: int, new_char: str) -> str:
    return s[:i] + new_char + s[i + 1:]


def last(xs: Sequence[A]) -> A:
    return xs[len(xs) - 1]


def empty_grid(rows: int, cols: int, init_value: A) -> list[list[A]]:
    grid = list[list[A]]()
    for _ in range(0, rows):
        grid.append([init_value] * cols)
    return grid


id = 0
def next_id() -> int:
    global id
    id += 1
    return id


def get_options(is_test:bool | None = None):
    parser = argparse.ArgumentParser()
    parser.add_argument('--test', dest='is_test', action='store_true')
    parser.add_argument('--part1', dest='part1_only', action='store_true')
    parser.add_argument('--part2', dest='part2_only', action='store_true')
    parser.add_argument('--input', dest='input_file')
    options = parser.parse_args()

    if is_test is not None:
        options.is_test = is_test

    if options.input_file is None:
        file_prefix = '-test' if options.is_test else ''
        options.input_file = os.path.join(
            'input', f'{os.path.splitext(parser.prog)[0]}{file_prefix}.txt')

    return options


def measure(f: Callable) -> tuple[Any, int]:
    start = time.time()
    result = f()
    end = time.time()
    return [result, end - start]


def read_lines(file: str) -> list[str]:
    return [line.rstrip('\n') for line in open(file).readlines()]


def run(part1=None, part2=None, process_input: Callable = None, input=None, test_intput=None, is_test=None,):
    options = get_options(is_test)
    input = test_intput if options.is_test else input
    if input is None:
        input_lines = read_lines(options.input_file)
        input = input_lines
        if process_input is not None:
            input = process_input(input_lines) if process_input.__code__.co_argcount == 1 else process_input(
                input_lines, options)

    input = input if type(input) is tuple else (input,)

    if part1 is not None and not options.part2_only:
        result, elapsed = measure(lambda: part1(*input))
        print(f"Part 1: {result} ({elapsed:.3f} sec)")
    if part2 is not None and not options.part1_only:
        result, elapsed = measure(lambda: part2(*input))
        print(f"Part 2: {result} ({elapsed:.3f} sec)")
