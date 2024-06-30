import { type Day } from "./common";
import _ from "lodash";

type Food = [Set<string>, Set<string>];
type Input = Food[];

function intersection<T>(s1: Set<T>, s2: Set<T>): Set<T> {
  const result = new Set<T>();
  for (const x of s1) {
    if (s2.has(x)) {
      result.add(x);
    }
  }

  return result;
}

function removeIngr(foods: Food[], ingr: string, allergen: string) {
  foods.forEach(([ingrs, als]) => {
    ingrs.delete(ingr);
    als.delete(allergen);
  });

  return foods;
}

function head<T>(s: Set<T>): T {
  return s.values().next().value!;
}

function findIngridientsWithAllergens(foods: Food[]): Record<string, string> {
  // copy before mutating
  foods = foods.map(([ingrs, allergens]) => [
    new Set(ingrs),
    new Set(allergens),
  ]);

  let allergens = _.chain(foods)
    .flatMap<string>(([, alls]) => Array.from(alls))
    .uniq()
    .value();

  const defined: Record<string, string> = {};

  while (allergens.length > 0) {
    const identifiedAllegens = allergens
      .map(
        (allergen) =>
          [
            allergen,
            _.chain(foods)
              .filter(([, alls]) => alls.has(allergen))
              .map(([ingrs]) => ingrs)
              .reduce(intersection)
              .value(),
          ] as const
      )
      .filter(([, ingrs]) => ingrs.size === 1)
      .map(([allergen, ingrs]) => [head(ingrs), allergen] as const);

    identifiedAllegens.forEach(([ingr, allergen]) => {
      defined[ingr] = allergen;
      removeIngr(foods, ingr, allergen);
    });

    if (identifiedAllegens.length === 0) {
      throw new Error("Cannot identify any more allergic ingridients");
    }

    allergens = _.without(
      allergens,
      ...identifiedAllegens.map(([, allergen]) => allergen)
    );
  }

  return defined;
}

const day21: Day<Input> = {
  parseInput: (txt) =>
    txt.split("\n").map((line) => {
      const [ingredientsStr, allergensStr] = line.split(" (contains ");
      const ingredients = new Set(ingredientsStr.split(" "));
      const allergens = new Set(
        allergensStr.substring(0, allergensStr.length - 1).split(", ")
      );
      return [ingredients, allergens];
    }),
  part1(foods) {
    const ingridientsWithAllergens = findIngridientsWithAllergens(foods);
    const allergicIngridients = new Set(Object.keys(ingridientsWithAllergens));

    return _.chain(foods)
      .flatMap(([ingrs]) => Array.from(ingrs))
      .filter((ingr) => !allergicIngridients.has(ingr))
      .size()
      .value();
  },
  part2(foods) {
    const ingridientsWithAllergens = findIngridientsWithAllergens(foods);
    return _.chain(ingridientsWithAllergens)
      .entries()
      .orderBy(([, allergen]) => allergen)
      .map(([ingr]) => ingr)
      .join(",")
      .value();
  },
};

export default day21;
