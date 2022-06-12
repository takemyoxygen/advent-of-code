mod day1;
mod solver;
mod utils;

use std::path::Path;
use std::env;
use std::collections::HashMap;

use solver::Day;

fn get_all_solvers() -> HashMap<u16, impl Day> {
    let mut solvers = HashMap::new();
    solvers.insert(1, day1::Day1);
    solvers
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let is_test = args.contains(&String::from("--test"));
    let explicit_day = args.into_iter().filter_map(|arg| str::parse::<u16>(&arg).ok()).next();
    let solvers = get_all_solvers();
    let day = explicit_day.as_ref().or_else(|| solvers.keys().max()).unwrap();
    let test_postfix = if is_test {"-test"} else {""};
    match solvers.get(day) {
        Some(solver) => {
            let answer = solver.solve(&Path::new(&format!("./input/day{}{}.txt", day, test_postfix)));
            println!("{}", answer);
        }
        _ => println!("You haven't solved day {} yet", day)
    }
}
