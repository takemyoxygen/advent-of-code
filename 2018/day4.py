from datetime import datetime
import re
from typing import TypeAlias
import core
from collections import defaultdict


# falls asleep | wakes up | (begins shift, 123)
GuardAction: TypeAlias = str | tuple[str, int]
GuardEvent: TypeAlias = tuple[datetime, GuardAction]


def get_action(action_string: str) -> GuardAction:
    match action_string:
        case "falls asleep" | "wakes up":
            return action_string
        case _:
            return ("begins shift", int(re.match('Guard #(\d+) begins shift', action_string).groups()[0]))


def parse_input(lines: list[str]) -> list[GuardEvent]:
    pattern = "^\[(.+)\] (.*)"
    return [
        (datetime.strptime(timestamp, '%Y-%m-%d %H:%M'), get_action(action))
        for timestamp, action in (re.match(pattern, line).groups() for line in lines)
    ]


def part1(events: list[GuardEvent]) -> int:
    events = sorted(events, key=lambda evt: evt[0])
    sleeps: dict[int, list[tuple[int, int]]] = defaultdict(list)
    guard = None
    started_sleeping = None

    for timestamp,action in events:
        match action:
            case ("begins shift", id):
                guard = id
            case "falls asleep":
                started_sleeping = timestamp.minute
            case "wakes up":
                sleeps[guard].append((started_sleeping, timestamp.minute))
                started_sleeping = None

    target_guard = max(sleeps, key=lambda id: sum(end - start for start, end in sleeps[id]))
    sleep_minutes = defaultdict(lambda: 0)
    for start,end in sleeps[target_guard]:
        for minute in range(start, end):
            sleep_minutes[minute] += 1

    max_minute = max(sleep_minutes, key=sleep_minutes.get)

    return target_guard * max_minute


core.run(part1, process_input=parse_input)
