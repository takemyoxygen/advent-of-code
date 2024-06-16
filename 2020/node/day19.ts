import { type Day } from "./common";
import _ from "lodash";

type RuleDef =
  | { type: "literal"; value: string }
  | { type: "nested"; alternatives: number[][] };

type RuleMap = Map<number, RuleDef>;

type Rule = (input: string, idx: number) => Iterable<{ next: number }>;

function literal(val: string): Rule {
  return function* (input, idx) {
    if (input.startsWith(val, idx)) {
      yield { next: idx + val.length };
    }
  };
}

function concat(rule1: Rule, rule2: Rule): Rule {
  return function* (input, idx) {
    for (const match of rule1(input, idx)) {
      yield* rule2(input, match.next);
    }
  };
}

function sequence(rules: Rule[]): Rule {
  return rules.reduce(concat);
}

function alternative(rules: Rule[]): Rule {
  return function* (input, idx) {
    for (const rule of rules) {
      yield* rule(input, idx);
    }
  };
}

function lazy(ruleDefs: RuleMap, ruleId: number): Rule {
  const definition = ruleDefs.get(ruleId)!;

  return (input, idx) => {
    let result;
    if (definition.type === "literal") {
      result = literal(definition.value);
    } else {
      result = alternative(
        definition.alternatives.map((alt) =>
          sequence(alt.map((id) => lazy(ruleDefs, id)))
        )
      );
    }
    return result(input, idx);
  };
}

function match(rule: Rule, input: string): boolean {
  for (const m of rule(input, 0)) {
    if (m.next === input.length) {
      return true;
    }
  }
  return false;
}

const day19: Day<[RuleMap, string[]]> = {
  parseInput(txt) {
    const [rulesTxt, messagesTxt] = txt.split("\n\n");

    const rules = new Map();

    rulesTxt.split("\n").forEach((line) => {
      const literalMatch = line.match(/(\d+): "(.+)"/);
      if (literalMatch != null) {
        rules.set(Number(literalMatch[1]), {
          type: "literal",
          value: literalMatch[2],
        });
        return;
      }

      const [keyStr, defStr] = line.split(": ");
      const alternatives = defStr
        .split(" | ")
        .map((altTxt) => altTxt.split(" ").map(Number));

      rules.set(Number(keyStr), { type: "nested", alternatives });
    });

    return [rules, messagesTxt.split("\n")];
  },
  part1([ruleDefs, messages]) {
    const rule = lazy(ruleDefs, 0);
    return messages.filter((msg) => match(rule, msg)).length;
  },
  part2([ruleDefs, messages]) {
    ruleDefs.set(8, { type: "nested", alternatives: [[42], [42, 8]] });
    ruleDefs.set(11, {
      type: "nested",
      alternatives: [
        [42, 31],
        [42, 11, 31],
      ],
    });
    const rule = lazy(ruleDefs, 0);
    return messages.filter((msg) => match(rule, msg)).length;
  },
};

export default day19;
