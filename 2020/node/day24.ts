import { partNotImplemented, type Day } from "./common";
import _ from "lodash";
import { PointSet, move, type Point } from "./coord2d";

const directions = ["e", "se", "sw", "w", "nw", "ne"] as const;
type Direction = (typeof directions)[number];
type Path = Direction[];

function movehex(p: Point, dir: Direction): Point {
  switch (dir) {
    case "w":
      return move(p, { x: -1, y: 0 });
    case "e":
      return move(p, { x: 1, y: 0 });
    case "se":
      return move(p, { x: p.y % 2 === 0 ? 1 : 0, y: 1 });
    case "sw":
      return move(p, { x: p.y % 2 === 0 ? 0 : -1, y: 1 });
    case "ne":
      return move(p, { x: p.y % 2 === 0 ? 1 : 0, y: -1 });
    case "nw":
      return move(p, { x: p.y % 2 === 0 ? 0 : -1, y: -1 });
  }
}

function adjacent(tile: Point): Point[] {
  return directions.map((dir) => movehex(tile, dir));
}

function flip(tiles: PointSet, toFlip: Point) {
  if (tiles.has(toFlip)) {
    tiles.delete(toFlip);
  } else {
    tiles.add(toFlip);
  }
}

function flipInitial(paths: Path[]): PointSet {
  const blacks = new PointSet();
  const start = { x: 0, y: 0 };

  paths
    .map((path) => path.reduce(movehex, start))
    .forEach((p) => flip(blacks, p));

  return blacks;
}

function day(blacks: PointSet): PointSet {
  const relevantTiles = _.chain(blacks.values())
    .flatMap(adjacent)
    .uniqBy(PointSet.key)
    .value();

  const newBlack = new PointSet();

  relevantTiles.forEach((tile) => {
    const adjBlacks = _.chain(adjacent(tile))
      .filter((t) => blacks.has(t))
      .size()
      .value();

    if (blacks.has(tile)) {
      if (!(adjBlacks === 0 || adjBlacks > 2)) {
        newBlack.add(tile);
      }
    } else {
      if (adjBlacks === 2) {
        newBlack.add(tile);
      }
    }
  });

  return newBlack;
}

const day24: Day<Path[]> = {
  parseInput: (txt) =>
    txt
      .split("\n")
      .map((line) =>
        Array.from(line.matchAll(/e|se|sw|w|nw|ne/g)).map(
          (match) => match[0] as Direction
        )
      ),
  part1(input) {
    return flipInitial(input).size();
  },
  part2(input) {
    const blacks = flipInitial(input);
    return _.range(0, 100)
      .reduce((acc) => day(acc), blacks)
      .size();
  },
};

export default day24;
