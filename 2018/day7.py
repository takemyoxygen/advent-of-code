import re
from core import run, fst
from collections import defaultdict
import heapq

def parse_input(lines: list[str], options) -> tuple[dict[str, set[str]], set[str], int, int]:
    pattern = '^Step (.) must be finished before step (.) can begin\.$'
    deps = map(lambda line: re.match(pattern, line).groups(), lines)

    deps_map = defaultdict(set)
    all_jobs = set()

    for dep, job in deps:
        deps_map[job].add(dep)
        all_jobs.add(job)
        all_jobs.add(dep)

    return deps_map, all_jobs, 2 if options.is_test else 5, 0 if options.is_test else 60


def get_next_jobs(deps: dict[str, set[str]], all: set[str]) -> list[str]:
    return sorted(job for job in all if not deps[job])


def complete_job(job: str, deps: dict[str, set[str]], remaining: set[str]) -> None:
    for deps_set in deps.values():
            deps_set.discard(job)
    remaining.remove(job)


def clone_deps(deps_map: dict[str, set[str]]) -> dict[str, set[str]]:
    copy = deps_map.copy()
    for job in copy:
        copy[job] = copy[job].copy()
    return copy


def part1(deps_map: dict[str, set[str]], all_jobs: set[str], _: int, __:int) -> str:
    remaining_jobs = all_jobs.copy()
    deps_map = clone_deps(deps_map)
    result = ''

    while remaining_jobs:
        next_job = fst(get_next_jobs(deps_map, remaining_jobs))
        complete_job(next_job, deps_map, remaining_jobs)
        result += next_job

    return result


def part2(deps_map: dict[str, set[str]], all_jobs: set[str], workers: int, extra_duration: int) -> str:
    remaining_jobs = all_jobs.copy()
    deps_map = clone_deps(deps_map)
    scheduled_jobs = set()

    times_queue = [0]
    job_completion_times = defaultdict(list)
    free_workers = workers

    time = 0

    while remaining_jobs:
        time = heapq.heappop(times_queue)

        for to_complete in job_completion_times[time]:
            complete_job(to_complete, deps_map, remaining_jobs)
            scheduled_jobs.remove(to_complete)
            free_workers += 1

        available_jobs = [job for job in get_next_jobs(deps_map, remaining_jobs) if job not in scheduled_jobs][:free_workers]
        for job in available_jobs:
            completion_time = time + extra_duration + (ord(job) - ord('A') + 1)
            job_completion_times[completion_time].append(job)
            free_workers -= 1
            heapq.heappush(times_queue, completion_time)
            scheduled_jobs.add(job)

    return time



run(part1, part2, process_input=parse_input)