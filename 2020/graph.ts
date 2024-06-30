import { Queue } from "@datastructures-js/queue";

export function bfs<TNode, TKey>(
  start: TNode,
  adjacent: (n: TNode) => Iterable<TNode>,
  key: (n: TNode) => TKey,
  visit: (n: TNode) => void
) {
  const queue = new Queue<TNode>();
  const visited = new Set<TKey>();

  queue.enqueue(start);
  while (queue.size() > 0) {
    const node = queue.dequeue();
    if (visited.has(key(node))) {
      continue;
    }

    visited.add(key(node));
    visit(node);

    for (const adj of adjacent(node)) {
      if (!visited.has(key(adj))) {
        queue.enqueue(adj);
      }
    }
  }
}
