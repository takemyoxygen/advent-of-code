use crate::{intcode, solver::Day};

pub struct Day5;

impl Day for Day5 {
    type Intermediate = Vec<i32>;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        intcode::read_program(file)
    }

    fn part1(program: &Self::Intermediate) -> String {
        let result = intcode::run_program(&program, vec![1]).ensure_terminated();
        result
            .output
            .iter()
            .map(|i| i.to_string())
            .collect::<Vec<String>>()
            .join("")
    }

    fn part2(program: &Self::Intermediate) -> String {
        let result = intcode::run_program(&program, vec![5]).ensure_terminated();
        result
            .output
            .iter()
            .map(|i| i.to_string())
            .collect::<Vec<String>>()
            .join("")
    }
}
