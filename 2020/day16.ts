import { type Day } from "./common";
import _ from "lodash";

type Range = { min: number; max: number };

type Input = {
  fields: Record<string, Range[]>;
  yours: number[];
  nearby: number[][];
};

function matchesAny(x: number, ranges: Range[]): boolean {
  return ranges.some(({ min, max }) => min <= x && x <= max);
}

function findSuitableFields(
  x: number,
  classes: Record<string, Range[]>
): string[] {
  return Object.entries(classes)
    .filter(([, ranges]) => matchesAny(x, ranges))
    .map(([field]) => field);
}

function isValid(ticket: number[], ranges: Range[]): boolean {
  return ticket.every((x) => matchesAny(x, ranges));
}

function first<T>(xs: Set<T>): T {
  return xs.values().next().value;
}

const day16: Day<Input> = {
  parseInput(txt) {
    const [classesStr, yoursStr, nearbyStr] = txt.split("\n\n");
    const classes: Record<string, Range[]> = _.chain(classesStr.split("\n"))
      .map((line) => {
        const m = line.match(/(.+): (\d+)-(\d+) or (\d+)-(\d+)/)!;
        return [
          m[1],
          [
            { min: Number(m[2]), max: Number(m[3]) },
            { min: Number(m[4]), max: Number(m[5]) },
          ],
        ] as const;
      })
      .fromPairs()
      .value();

    const yours = yoursStr.split("\n")[1].split(",").map(Number);

    const nearby = _.chain(nearbyStr.split("\n"))
      .drop(1)
      .map((line) => line.split(",").map(Number))
      .value();

    return { fields: classes, yours, nearby };
  },
  part1(input) {
    const allRanges = Object.values(input.fields).flat(1);

    return _.chain(input.nearby)
      .flattenDeep()
      .filter((x) => !matchesAny(x, allRanges))
      .sum()
      .value();
  },
  part2(input) {
    const allRanges = Object.values(input.fields).flat(1);
    const validTickets = [input.yours, ...input.nearby].filter((ticket) =>
      isValid(ticket, allRanges)
    );
    const size = input.yours.length;
    const places = new Array(input.yours.length);
    input.yours.forEach((_, idx) => (places[idx] = new Set()));

    const matchingFields = _.range(0, size).map(
      (pos) =>
        new Set(
          _.intersection(
            ...validTickets
              .map((ticket) => ticket[pos])
              .map((x) => findSuitableFields(x, input.fields))
          )
        )
    );

    const fieldToPos = _.chain(matchingFields)
      .flatMap((fields, idx) =>
        Array.from(fields.values()).map((field) => [field, idx] as const)
      )
      .groupBy(([field]) => field)
      .entries()
      .map(([field, pairs]) => ({
        field,
        positions: new Set(pairs.map((p) => p[1])),
      }))
      .value();

    const unique: typeof fieldToPos = [];
    let ambiguous = fieldToPos;

    do {
      const [newUniq, remainingAmbiguous] = _.partition(
        ambiguous,
        (rec) => rec.positions.size === 1
      );

      newUniq.forEach((u) => {
        const val = first(u.positions);

        remainingAmbiguous.forEach((r) => {
          if (r.positions.size === 0) {
            throw new Error("Unable to uniquely identify field position");
          }

          r.positions.delete(val);
        });
      });

      unique.push(...newUniq);
      ambiguous = remainingAmbiguous;
    } while (ambiguous.length !== 0);

    return unique
      .filter((rec) => rec.field.startsWith("departure"))
      .map((rec) => input.yours[first(rec.positions)])
      .reduce(_.multiply);
  },
};

export default day16;
