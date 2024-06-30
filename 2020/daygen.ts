const day = Number(process.argv[2]);

const sourceCode = `
import { partNotImplemented, type Day } from "./common";
import _ from "lodash";

const day${day}: Day<string> = {
  parseInput: (txt) => txt,
  part1: partNotImplemented,
  part2: partNotImplemented,
};

export default day${day};`;

async function createFileIfDoesNotExist(path: string, content: string) {
  if (await Bun.file(path).exists()) {
    console.log(`File "${path}" already exists. Skipping`);
  } else {
    console.log(`Created "${path}" with template code`);
    Bun.write(path, content);
  }
}

await createFileIfDoesNotExist(`./day${day}.ts`, sourceCode);
await createFileIfDoesNotExist(`./input/day${day}-test.txt`, "");
await createFileIfDoesNotExist(`./input/day${day}.txt`, "");
