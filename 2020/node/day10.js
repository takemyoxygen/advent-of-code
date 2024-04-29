import fs from "fs";

export function parseInput(content) {
  return content.split("\n").map(Number);
}

function readFile() {
  const file = process.argv[2];
  const content = fs.readFileSync(file, "utf-8");
  return content.split("\n").map(Number);
}

// const input = readFile();

export function part1(input) {
  input = input.toSorted((a, b) => a - b);

  let prev = 0;
  const diffs = {};
  for (const jolt of input) {
    const diff = jolt - prev;
    diffs[diff] = (diffs[diff] ?? 0) + 1;
    prev = jolt;
  }

  diffs[3] = (diffs[3] ?? 0) + 1;
  return diffs[1] * diffs[3];
}

export function part2(input) {
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
}

// console.log("Part 1:", part1(input));
// console.log("Part 2:", part2(input));
