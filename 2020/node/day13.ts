import { partNotImplemented, type Day } from "./common";
import _ from "lodash";

function gcdExt(a: bigint, b: bigint): [bigint, bigint, bigint] {
  if (a === 0n) {
    return [b, 0n, 1n];
  }

  const [gcd, x1, y1] = gcdExt(b % a, a);
  const x = y1 - (b / a) * x1;
  const y = x1;
  return [gcd, x, y];
}

// returns a smallest positive number X such X % base = m
function modpos(m: bigint, base: bigint): bigint {
  let result = m;
  while (result < 0) result += base;
  return result;
}

// https://www.geeksforgeeks.org/euclidean-algorithms-basic-and-extended/
// https://www.youtube.com/watch?v=ru7mWZJlRQg
function chinese(nums: bigint[], rems: bigint[]): bigint {
  const prod = nums.reduce((a, b) => a * b);
  let result = 0n;

  for (let i = 0; i < nums.length; i++) {
    const prodI = prod / nums[i];
    const [, invI] = gcdExt(prodI, nums[i]);
    result += rems[i] * prodI * invI;
  }

  return modpos(result % prod, prod);
}

const day13: Day<[number, number[]]> = {
  parseInput(txt) {
    const [departure, buses] = txt.split("\n");
    return [Number(departure), buses.split(",").map(Number)];
  },
  part1([departure, buses]) {
    return _.chain(buses)
      .filter((n) => !isNaN(n))
      .map((bus) => {
        const trips = _.round(departure / bus, 2);
        if (Math.round(trips) === trips) {
          return [0, bus];
        }
        return [Math.ceil(trips) * bus - departure, bus];
      })
      .minBy(([wait]) => wait)
      .reduce((a, b) => a * b)
      .value();
  },
  part2([, buses]) {
    const [nums, rems] = buses
      .map((bus, delay) => [bus, delay] as const)
      .filter(([bus]) => !isNaN(bus))
      .reduce<[bigint[], bigint[]]>(
        ([nums, rems], [bus, delay]) => {
          nums.push(BigInt(bus));
          rems.push(BigInt(-delay));
          return [nums, rems];
        },
        [[], []]
      );

    return chinese(nums, rems).toString();
  },
};

export default day13;
