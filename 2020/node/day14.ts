import { type Day } from "./common";
import _ from "lodash";

type Instruction =
  | { type: "mask"; value: string }
  | { type: "mem"; addr: number; value: BigInt };

function applyMask(
  val: number | BigInt,
  mask: string,
  applyBit: (valBit: string | undefined, maskBit: string) => string
): string {
  const valBinary = val.toString(2);
  let result = [];

  for (let i = 0; i < mask.length; i++) {
    const maskBit = mask[mask.length - 1 - i];
    const valBit: string | undefined = valBinary[valBinary.length - 1 - i];
    result.push(applyBit(valBit, maskBit));
  }

  return result.reverse().join("");
}

function applyValueMask(val: BigInt, mask: string): BigInt {
  const result = applyMask(val, mask, (valBit, maskBit) =>
    maskBit === "X" ? valBit ?? "0" : maskBit
  );

  return BigInt(`0b${result}`);
}

function applyAddressMask(val: number, mask: string): BigInt[] {
  const maskedAddress = applyMask(val, mask, (valBit, maskBit) =>
    maskBit === "0" ? valBit ?? "0" : maskBit === "1" ? "1" : "X"
  );

  const maskedArressArr = maskedAddress.split("");
  const addresses: BigInt[] = [];

  function enumerateAddresses(idx: number) {
    if (idx === maskedArressArr.length) {
      addresses.push(BigInt(`0b${maskedArressArr.join("")}`));
      return;
    }

    if (maskedArressArr[idx] !== "X") {
      enumerateAddresses(idx + 1);
      return;
    }

    maskedArressArr[idx] = "0";
    enumerateAddresses(idx + 1);

    maskedArressArr[idx] = "1";
    enumerateAddresses(idx + 1);

    maskedArressArr[idx] = "X";
  }

  enumerateAddresses(0);

  return addresses;
}

const day14: Day<Instruction[]> = {
  parseInput: (txt) =>
    txt.split("\n").map((line) => {
      if (line.startsWith("mask = ")) {
        return { type: "mask", value: line.replace("mask = ", "") };
      }

      const memMatch = line.match(/mem\[(\d+)\] = (\d+)/)!;
      return {
        type: "mem",
        addr: Number(memMatch[1]),
        value: BigInt(memMatch[2]),
      };
    }),
  part1(instructions) {
    let mask: string | null = null;
    const memory = new Map<number, BigInt>();
    instructions.forEach((instr) => {
      if (instr.type === "mask") {
        mask = instr.value;
      } else {
        memory.set(instr.addr, applyValueMask(instr.value, mask!));
      }
    });

    return _.chain(Array.from(memory.values())).sum().value().toString();
  },
  part2(instructions) {
    let mask: string | null = null;
    const memory = new Map<BigInt, BigInt>();
    instructions.forEach((instr) => {
      if (instr.type === "mask") {
        mask = instr.value;
      } else {
        const addresses = applyAddressMask(instr.addr, mask!);
        addresses.forEach((addr) => memory.set(addr, instr.value));
      }
    });

    return _.chain(Array.from(memory.values())).sum().value().toString();
  },
};

export default day14;
