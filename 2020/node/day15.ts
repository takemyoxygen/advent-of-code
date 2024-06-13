import { partNotImplemented, type Day } from "./common";
import _ from "lodash";

function play(numbers: number[], maxTurns: number): number {
  const spoken = new Map<number, number[]>();
  let lastSpoken;

  function speak(turn: number, n: number) {
    // console.log(`Turn ${turn}. Speaking ${n}`);
    const turns = spoken.get(n) ?? [];
    turns.push(turn);
    if (turns.length > 2) {
      turns.shift();
    }
    spoken.set(n, turns);

    lastSpoken = n;
  }

  numbers.forEach((n, idx) => speak(idx + 1, n));

  for (let turn = numbers.length + 1; turn <= maxTurns; turn++) {
    if (spoken.get(lastSpoken!)!.length === 1) {
      speak(turn, 0);
    } else {
      const [before, prev] = spoken.get(lastSpoken!)!;
      speak(turn, prev - before);
    }
  }

  return lastSpoken!;
}

const day15: Day<number[][]> = {
  parseInput: (txt) =>
    txt.split("\n").map((line) => line.split(",").map(Number)),
  part1: (cases) => cases.map((xs) => play(xs, 2020)),
  part2: (cases) => cases.map((xs) => play(xs, 30000000)),
};

export default day15;
