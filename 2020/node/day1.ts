import { readNumbers, type Day } from "./common";

const day1: Day<number[]> = {
  parseInput: readNumbers,
  part1(xs) {
    const set = new Set(xs);
    for (const x of xs) {
      const another = 2020 - x;
      if (set.has(another)) {
        return another * x;
      }
    }
    return "Not found";
  },
  part2(xs) {
    const set = new Set(xs);
    for (let i = 0; i < xs.length; i++) {
      for (let j = i + 1; j < xs.length; j++) {
        const another = 2020 - xs[i] - xs[j];
        if (set.has(another)) {
          return another * xs[i] * xs[j];
        }
      }
    }
    return "Not found";
  },
};

export default day1;
