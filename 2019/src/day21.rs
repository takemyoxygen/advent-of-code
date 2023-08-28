use itertools::{Either, Itertools};

use crate::intcode;
use crate::solver::Day;
use std::iter;

fn execute(program: &intcode::Program, instructions: Vec<&str>) -> String {
    let input = instructions
        .iter()
        .flat_map(|s| s.as_bytes().iter().map(|b| *b as i64).chain(iter::once(10)))
        .collect();

    let result = intcode::run_program(program, input);
    let (ascii, other): (Vec<_>, Vec<_>) =
        result
            .ensure_terminated()
            .output
            .iter()
            .partition_map(|b| match u8::try_from(*b) {
                Ok(ascii) => Either::Left(ascii),
                _ => Either::Right(*b),
            });

    if other.len() == 1 {
        return other[0].to_string();
    }

    return format!(
        "{}\nOther non-ASCII output:{:?}",
        String::from_utf8(ascii).unwrap(),
        other
    );
}

pub struct Day21;

impl Day for Day21 {
    type Intermediate = intcode::Program;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        intcode::read_program(file)
    }

    fn part1(program: &Self::Intermediate) -> String {
        let instructions = vec![
            "NOT A T", "OR T J", "NOT B T", "OR T J", "NOT C T", "OR T J", "AND D J", "WALK",
        ];
        execute(program, instructions)
    }

    fn part2(program: &Self::Intermediate) -> String {
        let instructions = vec![
            "NOT A T", "OR T J", "NOT B T", "OR T J", "NOT C T", "OR T J", "AND D J", "NOT E T",
            "NOT T T", "OR H T", "AND T J", "RUN",
        ];
        execute(program, instructions)
    }
}
