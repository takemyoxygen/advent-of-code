use itertools::Itertools;

use crate::{intcode, solver::Day};

pub struct Day9;

impl Day for Day9 {
    type Intermediate = intcode::Program;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        intcode::read_program(file)
    }

    fn part1(program: &Self::Intermediate) -> String {
        intcode::run_program(program, vec![1])
            .ensure_terminated()
            .output
            .iter()
            .map(|x| x.to_string())
            .join(",")
    }

    fn part2(program: &Self::Intermediate) -> String {
        intcode::run_program(program, vec![2])
            .ensure_terminated()
            .output
            .iter()
            .map(|x| x.to_string())
            .join(",")
    }
}
