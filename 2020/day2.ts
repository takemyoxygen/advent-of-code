import { type Day } from "./common";
import _ from "lodash";

type Input = Array<[Policy, string]>;

type Policy = {
  start: number;
  end: number;
  char: string;
};

function isValid1(policy: Policy, password: string): boolean {
  const count = _.chain(password)
    .filter((c) => c === policy.char)
    .size()
    .value();
  return policy.start <= count && count <= policy.end;
}

function isValid2(policy: Policy, password: string): boolean {
  const atStart = password[policy.start - 1] === policy.char;
  const atEnd = password[policy.end - 1] === policy.char;

  return (atStart && !atEnd) || (atEnd && !atStart);
}

function countValid(
  input: Input,
  isValid: (policy: Policy, password: string) => boolean
): number {
  return _.chain(input)
    .filter(([policy, password]) => isValid(policy, password))
    .size()
    .value();
}

const day2: Day<Input> = {
  parseInput(content) {
    const pattern = /(\d+)-(\d+) (.*): (.+)/;
    return content.split("\n").map((line) => {
      const matches = line.match(pattern);

      if (matches === null) throw new Error(`Invalid input "${line}"`);

      const policy: Policy = {
        start: Number(matches[1]),
        end: Number(matches[2]),
        char: matches[3],
      };

      return [policy, matches[4]];
    });
  },
  part1(input) {
    return countValid(input, isValid1);
  },
  part2(input) {
    return countValid(input, isValid2);
  },
};

export default day2;
