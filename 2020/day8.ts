import { type Day } from "./common";
import _ from "lodash";

type Instruction = [string, number];

enum TerminationStatus {
  InfiniteLoop,
  TerminatedOk,
}

type ProgramResult = [TerminationStatus, number];

function run(program: Instruction[]): ProgramResult {
  let acc = 0;
  let cmd = 0;
  const executed = new Set();

  while (true) {
    if (executed.has(cmd)) {
      return [TerminationStatus.InfiniteLoop, acc];
    }

    if (cmd === program.length) {
      return [TerminationStatus.TerminatedOk, acc];
    }

    executed.add(cmd);

    const [instr, arg] = program[cmd];
    switch (instr) {
      case "nop": {
        cmd++;
        break;
      }
      case "acc": {
        acc += arg;
        cmd++;
        break;
      }
      case "jmp": {
        cmd += arg;
        break;
      }
      default:
        throw new Error(`Unsupported instruction "${instr}"`);
    }
  }
}

function replaceAt<T>(xs: T[], index: number, newVal: T): T[] {
  const result = [...xs];
  result[index] = newVal;
  return result;
}

const day8: Day<Instruction[]> = {
  parseInput: (text) =>
    text.split("\n").map((line) => {
      const [instr, arg] = line.split(" ");
      return [instr, Number(arg)];
    }),
  part1(instructions) {
    const [, result] = run(instructions);
    return result;
  },
  part2(instructions) {
    return _.chain(instructions)
      .map(([instr, arg], idx) => {
        if (instr === "nop") return replaceAt(instructions, idx, ["jmp", arg]);
        if (instr === "jmp") return replaceAt(instructions, idx, ["nop", arg]);
        return null;
      })
      .filter((i) => i !== null)
      .map((prog) => run(prog as Instruction[]))
      .filter(([status]) => status === TerminationStatus.TerminatedOk)
      .first()
      .value()[1];
  },
};

export default day8;
