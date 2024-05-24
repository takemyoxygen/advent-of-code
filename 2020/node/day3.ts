import { type Day } from "./common";
import { move, type Point } from "./coord2d";

function countTrees(grid: string[][], slope: Point): number {
  let position: Point = { x: 0, y: 0 };
  let trees = 0;
  const width = grid[0].length;

  while (position.y < grid.length) {
    if (grid[position.y][position.x % width] === "#") {
      trees++;
    }
    position = move(position, slope);
  }

  return trees;
}

const day3: Day<string[][]> = {
  parseInput: (str) => str.split("\n").map((line) => line.split("")),
  part1(grid) {
    return countTrees(grid, { x: 3, y: 1 });
  },
  part2(grid) {
    return [
      { x: 1, y: 1 },
      { x: 3, y: 1 },
      { x: 5, y: 1 },
      { x: 7, y: 1 },
      { x: 1, y: 2 },
    ]
      .map((delta) => countTrees(grid, delta))
      .reduce((acc, res) => acc * res, 1);
  },
};

export default day3;
