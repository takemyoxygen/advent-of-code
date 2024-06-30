import { type Day } from "./common";
import _ from "lodash";
import { move, type Point } from "./coord2d";

const OCCUPIED = "#";
const FREE = "L";

enum OccupiedSeats {
  None,
  TooMany,
  Acceptable,
}

const steps = Array.from(
  (function* () {
    const deltas = [0, 1, -1];
    for (const dx of deltas)
      for (const dy of deltas) {
        if (dx !== 0 || dy !== 0) {
          yield { x: dx, y: dy };
        }
      }
  })()
);

function classifyOccupied(count: number, max: number): OccupiedSeats {
  if (count === 0) return OccupiedSeats.None;
  if (count >= max) return OccupiedSeats.TooMany;
  return OccupiedSeats.Acceptable;
}

function countAdjacentOccupied(
  start: Point,
  grid: string[],
  max: number
): OccupiedSeats {
  const occupiedCount = _.chain(steps)
    .map((dir) => move(start, dir))
    .filter((pos) => grid[pos.y]?.[pos.x] === OCCUPIED)
    .take(max)
    .size()
    .value();

  return classifyOccupied(occupiedCount, max);
}

function round(
  grid: string[],
  getOccupied: (pos: Point, grid: string[]) => OccupiedSeats
): string[] {
  const result = [];
  for (let y = 0; y < grid.length; y++) {
    let line = "";
    for (let x = 0; x < grid[y].length; x++) {
      const current = grid[y][x];
      let next;
      const occupied = getOccupied({ x, y }, grid);
      if (current === FREE && occupied === OccupiedSeats.None) {
        next = OCCUPIED;
      } else if (current === OCCUPIED && occupied === OccupiedSeats.TooMany) {
        next = FREE;
      } else {
        next = current;
      }
      line += next;
    }
    result.push(line);
  }
  return result;
}

function areEqual(g1: string[], g2: string[]): boolean {
  return _.chain(g1)
    .zip(g2)
    .every(([l1, l2]) => l1 === l2)
    .value();
}

function seesOccupiedInDirection(
  start: Point,
  dir: Point,
  grid: string[]
): boolean {
  let current = move(start, dir);
  while (true) {
    let seat = grid[current.y]?.[current.x];

    if (seat === OCCUPIED) return true;
    if (seat === FREE || seat === undefined) return false;

    current = move(current, dir);
  }
}

function countSeenOccupied(
  start: Point,
  grid: string[],
  max: number
): OccupiedSeats {
  const occupiedCount = _.chain(steps)
    .filter((dir) => seesOccupiedInDirection(start, dir, grid))
    .take(max)
    .size()
    .value();

  return classifyOccupied(occupiedCount, max);
}

function solve(
  initial: string[],
  getOccupied: (pos: Point, grid: string[]) => OccupiedSeats
): number {
  let current = initial;
  while (true) {
    const next = round(current, getOccupied);

    if (areEqual(next, current)) {
      return _.chain(current)
        .flatMap((l) => l.split(""))
        .filter((s) => s === OCCUPIED)
        .size()
        .value();
    }

    current = next;
  }
}

const day11: Day<string[]> = {
  parseInput: (txt) => txt.split("\n"),
  part1(grid) {
    return solve(grid, (pos, g) => countAdjacentOccupied(pos, g, 4));
  },
  part2(grid) {
    return solve(grid, (pos, g) => countSeenOccupied(pos, g, 5));
  },
};

export default day11;
