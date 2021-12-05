from typing import List, Union
import functools

PROGRAMS_COUNT = 16

Move = tuple[str, Union[int, tuple[int, int], tuple[str, str]]]


def parse_input(lines: List[str]) -> List[Move]:
    moves = []

    for line in lines:
        if line[0] == 's':
            moves.append(('spin', int(line[1:])))
        elif line[0] == 'x':
            [x1, x2] = line[1:].split('/')
            moves.append(('exchange', (int(x1), int(x2))))
        else:
            [x1, x2] = line[1:].split('/')
            moves.append(('partner', (x1, x2)))

    return moves


def get_programs(count: int) -> List[str]:
    a = ord('a')
    return [chr(a + x) for x in range(count)]


def swap(arr, i, j):
    arr[i], arr[j] = arr[j], arr[i]


def perform_exchange(programs: List[str], params: tuple[int, int]) -> List[str]:
    i, j = params
    swap(programs, i, j)
    return programs


def perform_spin(programs: List[str], spin: int) -> List[str]:
    return programs[-spin:] + programs[:-spin]


def perform_partner(programs: List[str], params: tuple[str, str]) -> List[str]:
    p1, p2 = params
    swap(programs, programs.index(p1), programs.index(p2))
    return programs


def perform_move(programs: List[str], move: Move) -> List[str]:
    type, params = move
    if type == 'spin':
        return perform_spin(programs, params)
    elif type == 'exchange':
        return perform_exchange(programs, params)
    else:
        return perform_partner(programs, params)


def dance(programs: List[str], moves: List[Move], times: int = 1) -> List[str]:
    def loop(progs: List[str], _) -> List[str]:
        return functools.reduce(perform_move, moves, progs)

    return functools.reduce(loop, range(times), programs.copy())


def split_moves(moves: List[Move]) -> tuple[List[Move], List[Move]]:
    partners = []
    rest = []
    for move in moves:
        dst = partners if move[0] == 'partner' else rest
        dst.append(move)
    return rest, partners


def get_partners_spec(partner_moves: List[Move]) -> dict[str, str]:
    programs = get_programs(PROGRAMS_COUNT)
    after_partner = dance(programs.copy(), partner_moves)
    return dict(zip(programs, after_partner))


def extend_partners_spec(spec: dict[str, str]) -> dict[str, List[str]]:
    result = {}
    for key in spec:
        swaps = [key]
        current = spec[key]
        while current != key:
            swaps.append(current)
            current = spec[current]
        result[key] = swaps
    return result


def apply_extended_partner_spec(programs: List[str], spec: dict[str, List[str]], times: int) -> List[str]:
    result = programs.copy()
    for i in range(len(result)):
        swaps = spec[programs[i]]
        result[i] = swaps[times % len(swaps)]
    return result


def get_positional_spec(positional_moves: List[Move]) -> List[int]:
    programs = get_programs(PROGRAMS_COUNT)
    after_moves = dance(programs.copy(), positional_moves)
    return [after_moves.index(c) for c in programs]


def apply_extended_positional_spec(programs: List[str], spec: List[List[int]], times: int) -> List[str]:
    result = programs.copy()
    for i in range(len(programs)):
        current_spec = spec[i]
        result[current_spec[times % len(current_spec)]] = programs[i]
    return result


def extend_positional_spec(spec: List[int]) -> List[List[int]]:
    results = []
    for i in range(len(spec)):
        positions = [i]
        current_pos = spec[i]
        while current_pos != i:
            positions.append(current_pos)
            current_pos = spec[current_pos]
        results.append(positions)
    return results


def dance_fast(programs: List[str], extended_positional_spec: List[List[int]], extended_partner_spec: dict[str, List[str]], times: int) -> List[str]:
    after_pos = apply_extended_positional_spec(programs, extended_positional_spec, times)
    return apply_extended_partner_spec(after_pos, extended_partner_spec, times)


moves = parse_input(open('input/day16.txt').read().split(','))
programs = get_programs(16)

positional_moves, partner_moves = split_moves(moves)
partner_spec = get_partners_spec(partner_moves)
positional_spec = get_positional_spec(positional_moves)
extended_positional_spec = extend_positional_spec(positional_spec)
extended_partner_spec = extend_partners_spec(partner_spec)


print('Part1', ''.join(dance_fast(programs.copy(), extended_positional_spec, extended_partner_spec, 1)))
print('Part2', ''.join(dance_fast(programs.copy(), extended_positional_spec, extended_partner_spec, int(1E9))))