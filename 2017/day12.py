from typing import List, Set
import collections


def parse_input(lines: List[str]) -> dict[int, List[int]]:
    graph = {}
    for line in lines:
        [node_str, adjacent_str] = line.split('<->')
        adjacent = [int(adj) for adj in adjacent_str.split(', ')]
        graph[int(node_str.strip())] = adjacent
    return graph


def bfs(graph: dict[int, List[int]], start: int) -> Set[int]:
    visited = set()
    queue = collections.deque([start])
    while queue:
        node = queue.popleft()
        if node not in visited:
            visited.add(node)
            for adj in graph.get(node, []):
                queue.append(adj)
    return visited


def get_connected_components(graph: dict[int, List[int]]) -> List[Set[int]]:
    remaining = set(graph.keys())
    components = []
    while remaining:
        start = remaining.pop()
        component = bfs(graph, start)

        for node in component:
            if node in remaining:
                remaining.remove(node)

        components.append(component)
    return components


lines = open('input/day12.txt').readlines()
graph = parse_input(lines)

components = get_connected_components(graph)

print('Part 1', len((next(filter(lambda comp: 0 in comp, components)))))
print('Part 2', len(components))
