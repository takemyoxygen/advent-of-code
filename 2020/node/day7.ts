import { Queue } from "@datastructures-js/queue";
import { partNotImplemented, type Day } from "./common";
import _ from "lodash";

type Edge = { qty: number; bag: string };
type Graph = Record<string, Edge[]>;

function addEdge(graph: Graph, from: string, edge: Edge) {
  const edges = (graph[from] = graph[from] ?? []);
  edges.push(edge);
}

function reverse(graph: Graph): Graph {
  const reversed: Graph = {};

  Object.entries(graph).forEach(([from, edges]) => {
    edges.forEach(({ qty, bag: to }) => {
      addEdge(reversed, to, { qty, bag: from });
    });
  });

  return reversed;
}

const day7: Day<Graph> = {
  parseInput(lines) {
    const graph: Graph = {};
    lines.split("\n").forEach((line) => {
      const [bagMatch, ...contentMatches] = Array.from(
        line.matchAll(/(\d+)? ?([a-z]+ [a-z]+) bags?/g)
      );
      const bag = bagMatch[2];
      contentMatches
        .map((m) =>
          m[2] === "no other" ? null : { qty: Number(m[1]), bag: m[2] }
        )
        .filter(Boolean)
        .forEach((edge) => addEdge(graph, bag, edge as Edge));
    });
    return graph;
  },
  part1(graph) {
    const reversed = reverse(graph);
    const queue = new Queue<string>();
    const visited = new Set();

    queue.enqueue("shiny gold");
    while (queue.size() > 0) {
      const bag = queue.dequeue();
      if (visited.has(bag)) {
        continue;
      }

      visited.add(bag);

      (reversed[bag] ?? [])
        .filter((edge) => !visited.has(edge.bag))
        .forEach((edge) => queue.enqueue(edge.bag));
    }

    return visited.size - 1;
  },
  part2(graph) {
    const cache: Record<string, number> = {};

    function countInside(bag: string): number {
      const cached = cache[bag];
      if (cached !== undefined) {
        return cached;
      }

      const count =
        (graph[bag] ?? [])
          .map(({ qty, bag: inner }) => countInside(inner) * qty)
          .reduce((acc, cnt) => acc + cnt, 0) + 1;

      cache[bag] = count;

      return count;
    }

    return countInside("shiny gold") - 1;
  },
};

export default day7;
