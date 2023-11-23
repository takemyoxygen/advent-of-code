mod day1;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;
mod day2;
mod day20;
mod day21;
mod day22;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;

mod coord;
mod intcode;
mod solver;
mod utils;

use std::collections::HashMap;
use std::env;
use std::path::Path;

use solver::Solvable;

fn get_all_solvers() -> HashMap<u16, Box<dyn Solvable>> {
    let mut solvers = HashMap::new();

    solvers.insert(1, Box::new(day1::Day1) as Box<dyn Solvable>);
    solvers.insert(2, Box::new(day2::Day2) as Box<dyn Solvable>);
    solvers.insert(3, Box::new(day3::Day3) as Box<dyn Solvable>);
    solvers.insert(4, Box::new(day4::Day4) as Box<dyn Solvable>);
    solvers.insert(5, Box::new(day5::Day5) as Box<dyn Solvable>);
    solvers.insert(6, Box::new(day6::Day6) as Box<dyn Solvable>);
    solvers.insert(7, Box::new(day7::Day7) as Box<dyn Solvable>);
    solvers.insert(8, Box::new(day8::Day8) as Box<dyn Solvable>);
    solvers.insert(9, Box::new(day9::Day9) as Box<dyn Solvable>);
    solvers.insert(10, Box::new(day10::Day10) as Box<dyn Solvable>);
    solvers.insert(11, Box::new(day11::Day11) as Box<dyn Solvable>);
    solvers.insert(12, Box::new(day12::Day12) as Box<dyn Solvable>);
    solvers.insert(13, Box::new(day13::Day13) as Box<dyn Solvable>);
    solvers.insert(14, Box::new(day14::Day14) as Box<dyn Solvable>);
    solvers.insert(15, Box::new(day15::Day15) as Box<dyn Solvable>);
    solvers.insert(16, Box::new(day16::Day16) as Box<dyn Solvable>);
    solvers.insert(17, Box::new(day17::Day17) as Box<dyn Solvable>);
    solvers.insert(18, Box::new(day18::Day18) as Box<dyn Solvable>);
    solvers.insert(19, Box::new(day19::Day19) as Box<dyn Solvable>);
    solvers.insert(20, Box::new(day20::Day20) as Box<dyn Solvable>);
    solvers.insert(21, Box::new(day21::Day21) as Box<dyn Solvable>);
    solvers.insert(22, Box::new(day22::Day22) as Box<dyn Solvable>);

    solvers
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let is_test = args.contains(&String::from("--test"));
    let part1_only = args.contains(&String::from("--part1"));
    let part2_only = args.contains(&String::from("--part2"));
    let explicit_day = args
        .into_iter()
        .filter_map(|arg| str::parse::<u16>(&arg).ok())
        .next();
    let solvers = get_all_solvers();
    let day = explicit_day
        .as_ref()
        .or_else(|| solvers.keys().max())
        .unwrap();
    let test_postfix = if is_test { "-test" } else { "" };
    match solvers.get(day) {
        Some(solver) => {
            let answer = solver.solve(
                &Path::new(&format!("./input/day{}{}.txt", day, test_postfix)),
                !part2_only,
                !part1_only,
            );
            println!("{}", answer);
        }
        _ => println!("You haven't solved day {} yet", day),
    }
}
