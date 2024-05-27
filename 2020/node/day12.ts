import { partNotImplemented, type Day } from "./common";
import _ from "lodash";
import { manhattan, move, type Point } from "./coord2d";

// clockwise, starting from East
const directions = {
  E: { x: 1, y: 0 },
  S: { x: 0, y: 1 },
  W: { x: -1, y: 0 },
  N: { x: 0, y: -1 },
};

const clockwiseDirections = [
  directions.E,
  directions.S,
  directions.W,
  directions.N,
];

function rotate({ x, y }: Point, angle: number, direction: "L" | "R"): Point {
  const rad = angle * (Math.PI / 180) * (direction === "R" ? 1 : -1);
  return {
    x: Math.round(x * Math.cos(rad) - y * Math.sin(rad)),
    y: Math.round(x * Math.sin(rad) + y * Math.cos(rad)),
  };
}

const day12: Day<[string, number][]> = {
  parseInput: (txt) =>
    txt.split("\n").map((line) => [line[0], Number(line.substring(1))]),
  part1(instructions) {
    const { pos: finalPos } = instructions.reduce(
      ({ pos, dir }, [action, arg]) => {
        if (action in directions) {
          return {
            pos: move(pos, directions[action as keyof typeof directions], arg),
            dir,
          };
        }
        if (action === "F") {
          return {
            pos: move(pos, clockwiseDirections[dir], arg),
            dir,
          };
        }

        if (action === "R") {
          return { pos, dir: (dir + arg / 90) % clockwiseDirections.length };
        }

        if (action === "L") {
          return {
            pos,
            dir:
              (clockwiseDirections.length + dir - arg / 90) %
              clockwiseDirections.length,
          };
        }

        throw new Error(`Unsupported action "${action}"`);
      },
      {
        pos: { x: 0, y: 0 },
        dir: 0,
      }
    );
    return manhattan(finalPos);
  },
  part2(instructions) {
    const { pos: finalPos } = instructions.reduce(
      ({ pos, way }, [action, arg]) => {
        if (action in directions) {
          return {
            way: move(way, directions[action as keyof typeof directions], arg),
            pos,
          };
        }

        if (action === "F") {
          return {
            pos: move(pos, way, arg),
            way,
          };
        }

        if (action === "L" || action === "R") {
          return {
            pos,
            way: rotate(way, arg, action),
          };
        }

        throw new Error(`Unsupported action "${action}"`);
      },
      {
        pos: { x: 0, y: 0 },
        way: { x: 10, y: -1 },
      }
    );

    return manhattan(finalPos);
  },
};

export default day12;
