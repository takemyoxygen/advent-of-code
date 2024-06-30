import { readNumbers, type Day } from "./common";

type TInput = number[];

const day10: Day<number[]> = {
  parseInput: readNumbers,

  part1(input: number[]) {
    input = input.toSorted((a, b) => a - b);

    let prev = 0;
    const diffs: Record<number, number> = {};
    for (const jolt of input) {
      const diff = jolt - prev;
      diffs[diff] = (diffs[diff] ?? 0) + 1;
      prev = jolt;
    }

    diffs[3] = (diffs[3] ?? 0) + 1;
    return diffs[1] * diffs[3];
  },

  part2(input: TInput) {
    input = [0, ...input].sort((a, b) => a - b);
    const results = new Array(input.length);
    results[results.length - 1] = 1;

    for (let i = results.length - 2; i >= 0; i--) {
      results[i] = 0;
      for (
        let follow = i + 1;
        follow < input.length && input[follow] - input[i] <= 3;
        follow++
      ) {
        results[i] += results[follow];
      }
    }

    return results[0];
  },
};

export default day10;
