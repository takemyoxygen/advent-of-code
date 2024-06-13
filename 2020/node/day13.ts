import { partNotImplemented, type Day } from "./common";
import _ from "lodash";

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
    const delays = buses
      .map((bus, delay) => [bus, delay] as const)
      .filter(([bus]) => !isNaN(bus))
      .map(([bus, delay]) => [BigInt(bus), BigInt(delay)]);

    return delays;

    const [step, offset] = _.maxBy(delays, ([bus]) => bus)!;
    let t = step;

    while (true) {
      const normalizedT = t - offset;
      if (
        delays.every(
          ([bus, delay]) => (normalizedT + delay) % bus === BigInt(0)
        )
      ) {
        return normalizedT;
      }
      t += step;
    }
  },
};

export default day13;
