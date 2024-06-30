import { type Day } from "./common";
import _ from "lodash";
import { move, type Point } from "./coord2d";
import { Queue } from "@datastructures-js/queue";

//up, right, bottom, left
type Border = [string, string, string, string];

type Matrix<T> = T[][];

type Variation = {
  content: Matrix<string>;
  border: Border;
};

type Tile = {
  id: number;
  variations: Variation[];
};

type FixedTile = {
  id: number;
  variation: Variation;
};

function empty<T>(size: number): Matrix<T> {
  const matrix = new Array(size);
  for (let i = 0; i < size; i++) {
    matrix[i] = new Array(size);
  }

  return matrix;
}

function transpose<T>(m: Matrix<T>): Matrix<T> {
  const result = empty<T>(m.length);

  for (let i = 0; i < m.length; i++)
    for (let j = 0; j < m.length; j++) {
      result[i][j] = m[j][i];
    }

  return result;
}

function flip<T>(m: Matrix<T>) {
  return m.toReversed();
}

function lastx<T>(xs: T[]): T {
  return xs[xs.length - 1];
}

function firstx<T>(xs: T[]): T {
  return xs[0];
}

function borders<T>(m: Matrix<T>): [T[], T[], T[], T[]] {
  return [m[0], m.map(lastx), m[m.length - 1], m.map(firstx)];
}

function variations<T>(m: Matrix<T>): Matrix<T>[] {
  function pair(m: Matrix<T>): [Matrix<T>, Matrix<T>] {
    const t = transpose(m);
    return [t, flip(t)] as const;
  }

  return _.range(0, 4).reduce<[Matrix<T>[], Matrix<T>]>(
    ([vars, current], _) => {
      const [t, f] = pair(current);
      vars.push(t, f);
      return [vars, f];
    },
    [[], m]
  )[0];
}

function align(tiles: [Point, FixedTile][]): FixedTile[][] {
  const offset = {
    x: -Math.min(...tiles.map(([p]) => p.x)),
    y: -Math.min(...tiles.map(([p]) => p.y)),
  };
  const tilesSide = Math.round(Math.sqrt(tiles.length));
  const aligned = empty<FixedTile>(tilesSide);

  for (const [pos, tile] of tiles) {
    const normalizedPos = move(pos, offset);
    aligned[normalizedPos.y][normalizedPos.x] = tile;
  }

  return aligned;
}

function positionTiles(tilesMap: Map<number, Matrix<string>>): FixedTile[][] {
  const oppositeBorders = [2, 3, 0, 1];
  const directions: Point[] = [
    { x: 0, y: -1 },
    { x: 1, y: 0 },
    { x: 0, y: 1 },
    { x: -1, y: 0 },
  ];

  const poskey = (p: Point) => `[${p.x}, ${p.y}]`;

  const tiles: Tile[] = Array.from(tilesMap.entries()).map(([id, content]) => ({
    id,
    variations: variations(content).map((variation) => ({
      content: variation,
      border: borders(variation).map((b) => b.join("")) as Border,
    })),
  }));

  function fixAlongBorder(
    border: string,
    borderIndex: number,
    excludeId: number
  ): FixedTile | null {
    return _.chain(tiles)
      .filter((t) => t.id !== excludeId)
      .map((t) => {
        const variation = t.variations.find(
          (v) => v.border.indexOf(border) === borderIndex
        );

        return variation == null ? null : { id: t.id, variation };
      })
      .filter((f) => f != null)
      .head()
      .value();
  }

  const fixed = new Map<string, [Point, FixedTile]>();
  const queue = new Queue<[Point, FixedTile]>();
  queue.enqueue([
    { x: 0, y: 0 },
    { id: tiles[0].id, variation: tiles[0].variations[0] },
  ]);

  while (!queue.isEmpty()) {
    const [pos, currentTile] = queue.dequeue();

    if (fixed.has(poskey(pos))) {
      continue;
    }

    fixed.set(poskey(pos), [pos, currentTile]);

    currentTile.variation.border.forEach((border, borderIdx) => {
      const nextPos = move(pos, directions[borderIdx]);
      const nextTile = fixAlongBorder(
        border,
        oppositeBorders[borderIdx],
        currentTile.id
      );

      const existing = fixed.get(poskey(nextPos))?.[1];

      if (
        nextTile != null &&
        existing != null &&
        !_.isEqual(nextTile.variation.border, existing.variation.border)
      ) {
        throw new Error(
          `Multiple tile variations for pos ${poskey(nextPos)}: tiles ${
            nextTile.id
          } and ${existing.id}`
        );
      }

      if (existing == null && nextTile != null) {
        queue.enqueue([nextPos, nextTile]);
      }
    });
  }

  return align(Array.from(fixed.values()));
}

function removeBorders<T>(m: Matrix<T>): Matrix<T> {
  return m.slice(1, m.length - 1).map((row) => row.slice(1, row.length - 1));
}

function flatten<T>(m: Matrix<Matrix<T>>): Matrix<T> {
  const innerSize = m[0][0].length;
  const result = empty<T>(m.length * innerSize);

  for (let x = 0; x < m.length; x++) {
    for (let y = 0; y < m.length; y++) {
      for (let iy = 0; iy < innerSize; iy++) {
        for (let ix = 0; ix < innerSize; ix++) {
          result[y * innerSize + iy][x * innerSize + ix] = m[y][x][iy][ix];
        }
      }
    }
  }

  return result;
}

function findSeaMonsters(m: Matrix<string>, monster: Point[]): Point[] {
  const monsterStarts: Point[] = [];
  for (let y = 0; y < m.length; y++) {
    for (let x = 0; x < m.length; x++) {
      const pos = { x, y };
      if (
        monster.map((m) => move(pos, m)).every(({ x, y }) => m[y]?.[x] === "#")
      ) {
        monsterStarts.push(pos);
      }
    }
  }

  return monsterStarts;
}

const day20: Day<Map<number, Matrix<string>>> = {
  parseInput(txt) {
    const tiles = new Map<number, Matrix<string>>();
    txt.split("\n\n").forEach((tile) => {
      const [idStr, ...contentLines] = tile.trim().split("\n");
      const id = Number(idStr.match(/\d+/)![0]);
      const content = contentLines.map((line) => line.split(""));
      tiles.set(id, content);
    });
    return tiles;
  },
  part1(tiles) {
    const positioned = positionTiles(tiles);
    const last = positioned.length - 1;

    return (
      positioned[0][0].id *
      positioned[0][last].id *
      positioned[last][last].id *
      positioned[last][0].id
    );
  },
  part2(tileInput) {
    const positioned = positionTiles(tileInput);
    const image = flatten(
      positioned.map((row) =>
        row.map((tile) => removeBorders(tile.variation.content))
      )
    );

    const monster = `                  # 
#    ##    ##    ###
 #  #  #  #  #  #   
`;

    const monsterCoords = monster
      .split("\n")
      .flatMap((line, y) =>
        line.split("").flatMap((s, x) => (s === "#" ? [{ x, y }] : []))
      );

    const hashes = _.sum(
      image.map((row) => row.filter((s) => s === "#").length)
    );

    const monstersCount = variations(image)
      .map((i) => findSeaMonsters(i, monsterCoords).length)
      .filter((cnt) => cnt > 0)[0];

    return hashes - monsterCoords.length * monstersCount;
  },
};

export default day20;
