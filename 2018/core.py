#!/usr/bin/env python3

import os
import argparse
from typing import Callable


def fst(x):
    return x[0]


def snd(x):
    return x[1]


def replace_at(s: str, i: int, new_char: str) -> str:
    return s[:i] + new_char + s[i + 1:]


def last(xs):
  return xs[len(xs) - 1]


def get_options():
    parser = argparse.ArgumentParser()
    parser.add_argument('--test', dest='is_test', action='store_true')
    parser.add_argument('--part1', dest='part1_only', action='store_true')
    parser.add_argument('--part2', dest='part2_only', action='store_true')
    parser.add_argument('--input', dest='input_file')

    options = parser.parse_args()
    if options.input_file is None:
        file_prefix = '-test' if options.is_test else ''
        options.input_file = os.path.join(
            'input', f'{os.path.splitext(parser.prog)[0]}{file_prefix}.txt')

    return options


def run(part1=None, part2=None, process_input: Callable = None):
    options = get_options()
    input_lines = open(options.input_file).readlines()

    input = input_lines
    if process_input is not None:
        input = process_input(input_lines) if process_input.__code__.co_argcount == 1 else process_input(input_lines, options)
    input = input if type(input) is tuple else (input,)
    
    if part1 is not None and not options.part2_only:
        print('Part 1:', part1(*input))
    if part2 is not None and not options.part1_only:
        print('Part 2:', part2(*input))
