import { type Day } from "./common";
import _ from "lodash";

const fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"];

type Validator = Record<string, (val: string) => boolean>;

function numberBetween(min: number, max: number) {
  return (val: string) => {
    const num = Number(val);
    return !isNaN(num) && min <= num && num <= max;
  };
}

const validator: Validator = {
  byr: numberBetween(1920, 2002),
  iyr: numberBetween(2010, 2020),
  eyr: numberBetween(2020, 2030),
  hgt(val) {
    const cm = val.match(/^(\d+)cm$/);
    if (cm != null) {
      return numberBetween(150, 193)(cm[1]);
    }

    const inches = val.match(/^(\d+)in$/);
    if (inches != null) {
      return numberBetween(59, 76)(inches[1]);
    }

    return false;
  },
  hcl: (val) => val.match(/^#[0-9a-f]{6}$/) != null,
  ecl: (val) =>
    val === "amb" ||
    val === "blu" ||
    val === "brn" ||
    val === "gry" ||
    val === "grn" ||
    val === "hzl" ||
    val === "oth",
  pid: (val) => val.match(/^\d{9}$/) != null,
};

function validate(passport: Record<string, string>) {
  return Object.entries(validator).every(
    ([field, rule]) => passport[field] != null && rule(passport[field])
  );
}

const day4: Day<Record<string, string>[]> = {
  parseInput(input) {
    return input.split("\n\n").map((psp) =>
      _.chain(psp.split(/\n|\s/))
        .filter((s) => s.length > 0)
        .map((s) => s.split(":"))
        .fromPairs()
        .value()
    );
  },
  part1(passports) {
    const requiredFields = _.without(fields, "cid");
    return passports.filter((psp) => requiredFields.every((f) => _.has(psp, f)))
      .length;
  },
  part2(passports) {
    return passports.filter(validate).length;
  },
};

export default day4;
