export type Day<T> = {
  parseInput(content: string): T;
  part1(input: T): number | string;
  part2(input: T): number | string;
};

export function readNumbers(content: string): number[] {
  return content.split("\n").map(Number);
}
