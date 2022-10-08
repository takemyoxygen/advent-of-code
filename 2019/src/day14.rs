use std::collections::HashMap;

use crate::{solver::Day, utils};

pub struct Day14;

type Chemical = String;

const SOURCE_CHEMICAL: &str = "ORE";

#[derive(Debug)]
pub struct Reaction {
    result_count: usize,
    ingridients: HashMap<Chemical, usize>,
}

fn parse_chemical(s: &str) -> (Chemical, usize) {
    let parts = s.trim().split(" ").collect::<Vec<&str>>();
    return (parts[1].to_string(), str::parse(parts[0]).unwrap());
}

fn find_ore_for(
    ch: &Chemical,
    required_count: usize,
    reactions: &HashMap<Chemical, Reaction>,
    leftovers: &mut HashMap<Chemical, usize>,
) -> usize {
    if ch == SOURCE_CHEMICAL {
        return required_count;
    }
    let mut required_count = required_count;

    let left = *leftovers.get(&ch.to_string()).unwrap_or(&0);
    let consumed = usize::min(left, required_count);
    required_count -= consumed;
    leftovers.entry(ch.to_string()).and_modify(|l| {
        *l -= consumed;
    });

    if required_count == 0 {
        return 0;
    }

    let reaction = reactions.get(ch).unwrap();
    let times = (required_count as f64 / reaction.result_count as f64).ceil() as usize;

    let new_left = reaction.result_count * times - required_count;
    leftovers
        .entry(ch.to_string())
        .and_modify(|l| {
            *l += new_left;
        })
        .or_insert(new_left);

    let result = reaction
        .ingridients
        .iter()
        .map(|(ingr, ingr_cnt)| find_ore_for(ingr, ingr_cnt * times, reactions, leftovers))
        .sum();

    result
}

impl Day for Day14 {
    type Intermediate = HashMap<Chemical, Reaction>;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        utils::read_lines(file)
            .map(|line| {
                let parts = line.split("=>").collect::<Vec<&str>>();
                let (result_chemical, result_count) = parse_chemical(parts[1]);

                let ingridients = parts[0]
                    .split(", ")
                    .map(|token| parse_chemical(token))
                    .collect::<HashMap<_, _>>();

                let reaction = Reaction {
                    result_count,
                    ingridients,
                };

                (result_chemical, reaction)
            })
            .collect::<HashMap<_, _>>()
    }

    fn part1(reactions: &Self::Intermediate) -> String {
        find_ore_for(&String::from("FUEL"), 1, reactions, &mut HashMap::new()).to_string()
    }

    fn part2(reactions: &Self::Intermediate) -> String {
        let mut leftovers = reactions
            .keys()
            .map(|ch| (ch.to_string(), 0))
            .collect::<HashMap<Chemical, usize>>();

        let ore_count = 1000000000000_usize;
        let fuel = String::from("FUEL");
        let ore_per_fule = find_ore_for(&fuel, 1, reactions, &mut HashMap::new());
        let init_fuel = ore_count / ore_per_fule;

        let mut ore_consumed = find_ore_for(&fuel, init_fuel, reactions, &mut leftovers);
        let mut fuel_produced = init_fuel;

        let mut next_fuel = init_fuel / 2;

        loop {
            let mut new_leftovers = leftovers.clone();
            let ore = find_ore_for(&fuel, next_fuel, reactions, &mut new_leftovers);

            if ore_consumed + ore <= ore_count {
                leftovers = new_leftovers;
                ore_consumed += ore;
                fuel_produced += next_fuel;
            } else {
                if next_fuel == 1 {
                    return fuel_produced.to_string();
                }

                next_fuel /= 2;
            }
        }
    }
}
