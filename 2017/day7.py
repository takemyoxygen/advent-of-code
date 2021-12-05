from typing import Dict, List, Optional
from itertools import groupby
import collections

Node = collections.namedtuple('Node', ['id', 'weight', 'children'])


def parse_input(input: str) -> List[Node]:
  def parse_weight(weight_str: str) -> int:
    return int(weight_str.strip("()"))

  def parse_line(line: str) -> Node:
    [id, weight, *rest] = line
    children = [c.strip(',') for c in rest[1:]]
    return Node(id, parse_weight(weight), children)

  tokenized_lines = list(map(lambda line: line.split(" "), input.splitlines()))
  return [parse_line(line) for line in tokenized_lines]


def day1(parsed_input: List[tuple[str, str, List[str]]]) -> str:
  roots = set(map(lambda x: x[0], parsed_input))
  for _, _, children in parsed_input:
    for child in children:
      roots.remove(child)
  return roots.pop()

Result = collections.namedtuple('Result', ['weight', 'unbalanced_child', 'node'])

def to_dict(xs, keyfunc):
  return dict(map(lambda x: (keyfunc(x), x), xs))


def find_unbalanced(results: List[Result], node_by_id: Dict[str, Node]) -> Optional[int]:
  unbalanced_child = next(filter(lambda res: res.unbalanced_child, results), None)
  if unbalanced_child:
    return unbalanced_child.unbalanced_child

  by_weight = collections.defaultdict(list)
  for result in results:
    by_weight[result.weight].append(result.node)

  if len(by_weight) < 2:
    return None

  [unbalanced_total_weight, others_weight] = sorted(by_weight.keys(), key=lambda weight: by_weight[weight], reverse=True)
  unbalanced_node = node_by_id[by_weight[unbalanced_total_weight][0]]
  return unbalanced_node.weight + others_weight - unbalanced_total_weight


def day2(root: str, parsed_input: List[Node]) -> int:
  node_by_id: Dict[str, Node] = to_dict(parsed_input, lambda node: node.id)

  def loop(node_id: str) -> Result:
    node = node_by_id[node_id]
    children_results = list(map(loop, node.children))
    total_weight = node.weight + sum(map(lambda r: r.weight, children_results))
    unbalanced_child = find_unbalanced(children_results, node_by_id)

    return Result(total_weight, unbalanced_child, node_id)

  result = loop(root)
  return result.unbalanced_child

input = open("input/day7.txt").read()


parsed_input = parse_input(input)
root = day1(parsed_input)
print("Day1", root)
print("Day2", day2(root, parsed_input))