import { partNotImplemented, type Day } from "./common";
import _ from "lodash";

type Point = number[];
type ActivePoints = Map<number, Point>;

const key = (p: Point) =>
  p.reduce((acc, x, idx) => acc + Math.pow(100, idx) * x, 0);

function neighborOffsets(size: number): Point[] {
  let results: Point[] = [[]];
  const offsets = [0, 1, -1];

  for (let i = 0; i < size; i++) {
    results = results.flatMap((r) => offsets.map((d) => [...r, d]));
  }

  return results.filter((offset) => !offset.every((d) => d === 0));
}

function move(p: Point, delta: Point): Point {
  return p.map((c, idx) => c + delta[idx]);
}

function neighbors(p: Point, offsets: Point[]): Point[] {
  return neighborOffsets(p.length).map((d) => move(p, d));
}

function step(
  active: ActivePoints,
  neighbors: (p: Point) => Point[]
): ActivePoints {
  const next = new Map();

  for (const point of active.values()) {
    const [activeNeighbors, inactiveNeighbors] = _.partition(
      neighbors(point),
      (p) => active.has(key(p))
    );

    if (activeNeighbors.length === 2 || activeNeighbors.length == 3) {
      next.set(key(point), point);
    }

    for (const inact of inactiveNeighbors) {
      if (neighbors(inact).filter((p) => active.has(key(p))).length === 3) {
        next.set(key(inact), inact);
      }
    }
  }

  return next;
}

const day17: Day<[number, number][]> = {
  parseInput(txt) {
    const result: [number, number][] = [];
    txt.split("\n").forEach((line, y) => {
      for (let x = 0; x < line.length; x++) {
        if (line[x] === "#") {
          result.push([x, y] as const);
        }
      }
    });

    return result;
  },
  part1(input) {
    const offsets = neighborOffsets(3);
    const getNeighbors = (p: Point) => neighbors(p, offsets);

    const active3d = input
      .map((a) => [...a, 0])
      .reduce((m, pos) => m.set(key(pos), pos), new Map());

    return Array.from(
      _.range(0, 6)
        .reduce((set) => step(set, getNeighbors), active3d)
        .values()
    ).length;
  },
  part2(input) {
    const offsets = neighborOffsets(4);
    const getNeighbors = (p: Point) => neighbors(p, offsets);

    const active4d = input
      .map((a) => [...a, 0, 0])
      .reduce((m, pos) => m.set(key(pos), pos), new Map());

    return Array.from(
      _.range(0, 6)
        .reduce((set) => step(set, getNeighbors), active4d)
        .values()
    ).length;
  },
};

export default day17;
