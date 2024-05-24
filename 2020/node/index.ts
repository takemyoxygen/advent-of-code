import type { Day } from "./common";

const day = process.argv[2];
if (!day) throw new Error(`Enter day to run the solution for!`);

const file = process.argv[3] ?? `./input/day${day}.txt`;
const inputContent = await Bun.file(file).text();

const solution: Day<any> = (await import(`./day${day}.ts`)).default;

const input = solution.parseInput(inputContent);

console.log(`Day ${day}:`);
console.log("Part 1:", solution.part1(input));
console.log("Part 2:", solution.part2(input));
