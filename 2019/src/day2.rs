use crate::intcode;
use crate::solver::Day;
use std::path::Path;

pub struct Day2;

fn execute_program(input: &intcode::Program, noun: i32, verb: i32) -> i32 {
    let mut program = input.clone();
    program[1] = noun;
    program[2] = verb;
    let result = intcode::run_program(&program, Vec::new()).ensure_terminated();
    return result.program[0];
}

impl Day for Day2 {
    type Intermediate = intcode::Program;

    fn process(file: &Path) -> Self::Intermediate {
        intcode::read_program(file)
    }

    fn part1(input: &Self::Intermediate) -> std::string::String {
        execute_program(&input, 12, 2).to_string()
    }
    fn part2(input: &Self::Intermediate) -> std::string::String {
        let target = 19690720;
        for noun in 0..100 {
            for verb in 0..100 {
                if execute_program(&input, noun, verb) == target {
                    return (100 * noun + verb).to_string();
                }
            }
        }
        return String::new();
    }
}
