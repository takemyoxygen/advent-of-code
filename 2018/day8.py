from typing import Callable, NamedTuple, TypeVar
import core


class TreeNode(NamedTuple):
    metadata: list[str]
    children: list["TreeNode"]


T = TypeVar('T')


def read_tree(input: list[str]) -> TreeNode:

    def read_int(position: int) -> tuple[int, int]:
        return input[position], (position + 1)

    def read_many(position: int, count: int, read_one: Callable[[int], tuple[T, int]]) -> tuple[list[T], int]:
        items = []
        for _ in range(0, count):
            item, position = read_one(position)
            items.append(item)
        return items, position

    def read_node(position: int) -> tuple[TreeNode, int]:
        children_count, position = read_int(position)
        metadata_count, position = read_int(position)
        children, position = read_many(position, children_count, read_node)
        metadata, position = read_many(position, metadata_count, read_int)
        return TreeNode(metadata, children), position

    return read_node(0)[0]


def parse_input(lines: list[str]) -> list[int]:
    input = list(map(int, lines[0].split(" ")))
    return read_tree(input)


def part1(root: TreeNode) -> int:
    def sum_tree(node: TreeNode) -> int:
        return sum(node.metadata) + sum(map(sum_tree, node.children))

    return sum_tree(root)


def part2(root: TreeNode) -> int:

    def get_value(node: TreeNode) -> int:
        return sum(
            get_value(node.children[index]) for
            index in (meta - 1 for meta in node.metadata) if 0 <= index < len(node.children)) \
            if node.children \
            else sum(node.metadata)

    return get_value(root)


core.run(part1, part2, process_input=parse_input)
