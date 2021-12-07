from datetime import datetime
import re
from typing import TypeAlias
import core
from collections import defaultdict


GuardEvent: TypeAlias = tuple[datetime, str]

# guard ID -> list of minutes asleep per minute 0-59
SleepMatrix: TypeAlias = dict[int, list[int]]


def parse_input(lines: list[str]) -> list[GuardEvent]:
    pattern = "^\[(.+)\] (.*)"
    return [
        (datetime.strptime(timestamp, '%Y-%m-%d %H:%M'), action)
        for timestamp, action in (re.match(pattern, line).groups() for line in lines)
    ]


def get_sleep_minutes_matrix(lines: list[str]) -> SleepMatrix:
    events = sorted(parse_input(lines), key= lambda evt: evt[0])
    sleep_matrix = defaultdict(lambda: [0] * 60)
    guard = None
    started_sleeping = None

    for timestamp,action in events:
        match action:
            case "falls asleep":
                started_sleeping = timestamp.minute
            case "wakes up":
                for minute in range(started_sleeping, timestamp.minute):
                    sleep_matrix[guard][minute] += 1
                started_sleeping = None
            case _:
                guard = int(re.match('Guard #(\d+) begins shift', action).groups()[0])

    return sleep_matrix


def part1(sleep_matrix: SleepMatrix) -> int:
    target_guard = max(sleep_matrix, key=lambda guard: sum(sleep_matrix[guard]))
    max_minute = max(range(0, 60), key=lambda minute: sleep_matrix[target_guard][minute])
    return target_guard * max_minute


def part2(sleep_matrix: SleepMatrix) -> int:
    result = 0
    max_slept = 0
    for guard in sleep_matrix:
        for minute in range(len(sleep_matrix[guard])):
            if sleep_matrix[guard][minute] > max_slept:
                max_slept = sleep_matrix[guard][minute]
                result = guard * minute
    return result


core.run(part1, part2, process_input=get_sleep_minutes_matrix)
