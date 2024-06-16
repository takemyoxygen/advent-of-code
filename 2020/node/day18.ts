import { type Day } from "./common";
import _ from "lodash";

type Expr = (number | string)[];
type Input = Expr[];

function asNumber(x: string): number | undefined {
  const n = Number(x);
  return isNaN(n) ? undefined : n;
}

const operations: Record<string, (n: number, n2: number) => number> = {
  "+": _.add,
  "*": _.multiply,
};

function evalExprNoPriority(expr: Expr, start: number): [number, number] {
  let acc = 0;
  let op = "+";

  let pos = start;
  while (pos < expr.length) {
    if (typeof expr[pos] === "number") {
      acc = operations[op](acc, expr[pos] as number);
      pos++;
    } else if (_.has(operations, expr[pos])) {
      op = expr[pos] as string;
      pos++;
    } else if (expr[pos] === "(") {
      const [innerRes, innerEnd] = evalExprNoPriority(expr, pos + 1);
      acc = operations[op](acc, innerRes);
      pos = innerEnd;
    } else if (expr[pos] === ")") {
      return [acc, pos + 1];
    }
  }

  return [acc, pos];
}

function evalExprAddFirst(expr: Expr, start: number): [number, number] {
  let pos = start;
  let op = undefined;
  let prev: number[] = [];

  while (pos < expr.length) {
    if (typeof expr[pos] === "number") {
      if (op === "+") {
        prev.push(prev.pop()! + (expr[pos] as number));
      } else {
        prev.push(expr[pos] as number);
      }
      pos++;
    } else if (_.has(operations, expr[pos])) {
      op = expr[pos] as string;
      pos++;
    } else if (expr[pos] === "(") {
      const [innerRes, innerEnd] = evalExprAddFirst(expr, pos + 1);
      if (op === "+") {
        prev.push(prev.pop()! + (innerRes as number));
      } else {
        prev.push(innerRes);
      }
      pos = innerEnd;
    } else if (expr[pos] === ")") {
      pos++;
      break;
    }
  }

  return [prev.reduce(_.multiply, 1), pos];
}

function evaluate(
  expr: Expr,
  inner: (expr: Expr, start: number) => [number, number]
): number {
  const [result, end] = inner(expr, 0);
  if (end < expr.length) {
    throw new Error("Unable to eval the expression completely");
  }

  return result;
}

const parseExpr = (line: string): Expr =>
  line
    .replaceAll(" ", "")
    .split("")
    .map((c) => asNumber(c) ?? c);

const day18: Day<Input> = {
  parseInput: (txt) => txt.split("\n").map(parseExpr),
  part1: (exprs) =>
    _.chain(exprs)
      .map((expr) => evaluate(expr, evalExprNoPriority))
      .sum()
      .value(),
  part2: (exprs) =>
    _.chain(exprs)
      .map((expr) => evaluate(expr, evalExprAddFirst))
      .sum()
      .value(),
};

export default day18;
