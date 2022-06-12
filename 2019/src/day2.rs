use crate::utils::read_lines;
use crate::solver::Day;
use std::path::Path;

pub struct Day2;

type Program = Vec<usize>;

fn process_instruction(program:&mut Program, pos: usize) -> bool {
    match program[pos] {
        1 => {
            let dst = program[pos + 3];
            program[dst] = program[program[pos + 1]] + program[program[pos + 2]];
            true
        },
        2 => {
            let dst = program[pos + 3];
            program[dst] = program[program[pos + 1]] * program[program[pos + 2]];
            true
        }
        _ => false
    }
}

fn execute_program(input: &Program, noun: usize, verb: usize) -> usize {
    let mut program = input.clone();
    let mut pos = 0;
    program[1] = noun;
    program[2] = verb;
    while process_instruction(&mut program, pos) {
        pos += 4
    }
    program[0]
}

impl Day for Day2 {
    type Intermediate = Program;

    fn process(file: &Path) -> Self::Intermediate {
        let line = read_lines(file).next().unwrap();
        line.split(",")
            .map(|token| str::parse::<usize>(token).unwrap())
            .collect()
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
