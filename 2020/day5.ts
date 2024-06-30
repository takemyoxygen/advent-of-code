import { type Day } from "./common";
import _ from "lodash";

type Range = [number, number];

function reduce(range: Range, lo: boolean): Range {
  const size = (range[1] - range[0] + 1) / 2;
  return lo ? [range[0], range[0] + size - 1] : [range[1] - size + 1, range[1]];
}

function seatID(code: string): number {
  const rowCode = code.substring(0, 7).split("");
  const colCode = code.substring(7).split("");

  const [row] = rowCode.reduce<Range>(
    (range, ch) => reduce(range, ch === "F"),
    [0, 127]
  );

  const [col] = colCode.reduce<Range>(
    (range, ch) => reduce(range, ch === "L"),
    [0, 7]
  );

  return row * 8 + col;
}

const day5: Day<string[]> = {
  parseInput: (txt) => txt.split("\n"),
  part1(lines) {
    return _.chain(lines).map(seatID).max().value();
  },
  part2(lines) {
    const ordered = _.chain(lines).map(seatID).orderBy().value();
    for (let i = 0; i < ordered.length - 1; i++) {
      if (ordered[i + 1] - ordered[i] === 2) {
        return ordered[i] + 1;
      }
    }

    return "Not found";
  },
};

export default day5;
