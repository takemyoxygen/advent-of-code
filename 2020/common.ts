export type Day<T> = {
  parseInput(content: string): T;
  part1(input: T): any;
  part2(input: T): any;
};

export function readNumbers(content: string): number[] {
  return content.split("\n").map(Number);
}

export function partNotImplemented<T>(input: T) {
  return "Not implemented";
}
