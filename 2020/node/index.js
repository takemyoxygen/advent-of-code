import fs from "fs";

const day = process.argv[2];
const file = process.argv[3] ?? `./input/day${day}.txt`;
const inputContent = fs.readFileSync(file, "utf-8");

const { parseInput, part1, part2 } = await import(`./day${day}.js`);

const input = parseInput(inputContent);

console.log(`Day ${day}:`);
console.log("Part 1:", part1(input));
console.log("Part 2:", part2(input));
