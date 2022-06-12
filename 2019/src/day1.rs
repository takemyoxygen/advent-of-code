use crate::solver::Day;
use crate::utils::read_lines;
use std::path::Path;

pub struct Day1;

fn calc_fuel(mass: &i32) -> i32 {
    (mass / 3) - 2
}

fn calc_fuel_rec(mass: &i32) -> i32 {
    let next_fuel = calc_fuel(mass);
    if next_fuel > 0 {
        next_fuel + calc_fuel_rec(&next_fuel)
    } else {
        0
    }
}

impl Day for Day1 {
    type Intermediate = Vec<i32>;

    fn process(file: &Path) -> Self::Intermediate {
        let lines = read_lines(file);
        lines
            .filter(|line| line.is_ok())
            .map(|line| str::parse::<i32>(&line.unwrap()).unwrap())
            .collect()
    }

    fn part1(input: &Self::Intermediate) -> String {
        input.into_iter().map(calc_fuel).sum::<i32>().to_string()
    }

    fn part2(input: &Self::Intermediate) -> String {
        input
            .into_iter()
            .map(calc_fuel_rec)
            .sum::<i32>()
            .to_string()
    }
}
