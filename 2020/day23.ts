import { type Day } from "./common";
import _ from "lodash";

type Node = { value: number; next: Node };

function toArray(start: Node): number[] {
  const vals = [];

  let current = start;
  do {
    vals.push(current.value);
    current = current.next;
  } while (current !== start);

  return vals;
}

function move(
  current: Node,
  min: number,
  max: number,
  nodesByVal: Record<number, Node>
): Node {
  const toMove = [current.next, current.next.next, current.next.next.next];
  current.next = _.last(toMove)!.next;

  let dstVal = current.value;
  do {
    dstVal--;
    if (dstVal < min) {
      dstVal = max;
    }
  } while (toMove.some((n) => n.value === dstVal));

  const dest = nodesByVal[dstVal]!;
  _.last(toMove)!.next = dest.next;
  dest.next = toMove[0];

  return current.next;
}

function play(numbers: number[], rounds: number): Record<number, Node> {
  const min = 1;
  const max = numbers.length;

  const nodes = numbers.reduce<Node[]>((acc, num) => {
    const node = { value: num, next: null } as any as Node;
    if (acc.length > 0) {
      acc[acc.length - 1].next = node;
    }
    acc.push(node);
    return acc;
  }, []);

  nodes[nodes.length - 1].next = nodes[0];

  const nodesByVal = _.keyBy(nodes, (n) => n.value);

  let current = nodes[0];

  for (let i = 0; i < rounds; i++) {
    current = move(current, min, max, nodesByVal);
  }

  return nodesByVal;
}

const day23: Day<number[]> = {
  parseInput: (txt) => txt.split("").map(Number),
  part1(numbers) {
    const nodes = play(numbers, 100);
    return _.chain(toArray(nodes[1])).drop(1).join("").value();
  },
  part2(numbers: number[]) {
    const extended = [...numbers];
    for (let i = Math.max(...extended) + 1; i <= 1000000; i++) {
      extended.push(i);
    }
    const nodes = play(extended, 10000000);
    const one = nodes[1];
    return one.next.value * one.next.next.value;
  },
};

export default day23;
