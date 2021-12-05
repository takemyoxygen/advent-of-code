import re
from typing import List, NamedTuple, Union, Set
import math
from collections import defaultdict

Vector = tuple[int, int, int]
Particle = NamedTuple('Particle',
                      [('index', int), ('position', Vector), ('velocity', Vector), ('acceleration', Vector)])


def parse_input(lines: List[str]) -> List[Particle]:
    vector_pattern = '<(-?\d+),(-?\d+),(-?\d+)>'
    line_pattern = f"p={vector_pattern}, v={vector_pattern}, a={vector_pattern}"
    result = []
    for index, line in enumerate(lines):
        numbers = [int(num) for num in re.match(line_pattern, line).groups()]
        result.append(Particle(index, tuple(numbers[0:3]), tuple(numbers[3:6]), tuple(numbers[6:])))
    return result


def solve(a: float, b: float, c: float) -> List[float]:
    if a == 0:
        return [-c / b] if b != 0 else []

    d = b ** 2 - 4 * a * c
    if d < 0:
        return []

    x1 = (-b + math.sqrt(d)) / (2 * a)
    x2 = (-b - math.sqrt(d)) / (2 * a)
    return [x1, x2] if x1 != x2 else [x1]


def find_intersection(p1: Particle, p2: Particle) -> Union[float, None]:
    x_intersections = solve(
        (p1.acceleration[0] - p2.acceleration[0]) / 2,
        p1.velocity[0] - p2.velocity[0] + (p1.acceleration[0] - p2.acceleration[0]) / 2,
        p1.position[0] - p2.position[0])
    return min((time for time in x_intersections
                if time >= 0 and
                all(map(lambda i: abs(coord_at(time, p1, i) - coord_at(time, p2, i)) < 0.01, range(1, 3)))),
               default=None)


def get_particle_intersections(particles: List[Particle]) -> dict[float, Set[int]]:
    intersections = defaultdict(set)
    for i in range(len(particles)):
        for j in range(i + 1, len(particles)):
            intersection_time = find_intersection(particles[i], particles[j])
            if intersection_time is not None:
                intersections[intersection_time].add(particles[i].index)
                intersections[intersection_time].add(particles[j].index)
    return intersections


def find_remaining_particles(intersections: dict[float, Set[int]], particles_count: int) -> Set[int]:
    remaining_particles = set(range(particles_count))
    for time in sorted(intersections.keys()):
        collided = intersections[time]
        if len(remaining_particles.intersection(collided)) > 1:
            for p in collided:
                remaining_particles.remove(p)
    return remaining_particles


def coord_at(time: float, particle: Particle, coord: int) -> int:
    return particle.position[coord] + particle.velocity[coord] * time + (
                particle.acceleration[coord] * time * (time + 1) / 2)


def distance_at(time: int, particle: Particle) -> int:
    return sum(map(lambda i: abs(coord_at(time, particle, i)), range(3)))


def part1(particles: List[Particle]) -> Particle:
    time = 1000000000
    return min(particles, key=lambda part: distance_at(time, part))


def part2(particles: List[Particle]) -> int:
    intersections = get_particle_intersections(particles)
    remaining = find_remaining_particles(intersections, len(particles))
    return len(remaining)


lines = open('input/day20.txt').readlines()
particles = parse_input(lines)
print('Part 1:', part1(particles))
print('Part 2:', part2(particles))
