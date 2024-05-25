import { type Day } from "./common";
import _ from "lodash";

const day6: Day<string[][]> = {
  parseInput: (txt) => txt.split("\n\n").map((ans) => ans.split("\n")),
  part1: (groups) =>
    _.chain(groups)
      .map((gr) =>
        _.chain(gr)
          .flatMap((l) => l.split(""))
          .uniq()
          .size()
          .value()
      )
      .sum()
      .value(),
  part2: (groups) =>
    _.chain(groups)
      .map((gr) =>
        _.chain(gr)
          .flatMap((l) => l.split(""))
          .countBy(_.identity)
          .filter((x) => x === gr.length)
          .size()
          .value()
      )
      .sum()
      .value(),
};

export default day6;
