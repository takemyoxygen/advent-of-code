import { type Day } from "./common";
import _ from "lodash";

function loop(val: number, subj: number): number {
  return (val * subj) % 20201227;
}

function findLoopSize(target: number): number {
  let size = 0;
  let value = 1;
  while (value !== target) {
    value = loop(value, 7);
    size++;
  }
  return size;
}

function transform(subj: number, loopSize: number): number {
  let value = 1;
  for (let i = 0; i < loopSize; i++) {
    value = loop(value, subj);
  }

  return value;
}

const day25: Day<[number, number]> = {
  parseInput: (txt) => txt.split("\n").map(Number) as [number, number],
  part1([publicKey1, publicKey2]) {
    const loopSize = findLoopSize(publicKey1);
    return transform(publicKey2, loopSize);
  },
  part2: () => "N/A",
};

export default day25;
