use itertools::Itertools;

use crate::{intcode, solver::Day};

pub struct Day7;

fn run_program_for_part1(program: &intcode::Program, seq: Vec<i64>) -> i64 {
    let mut input = 0;

    for i in 0..seq.len() {
        let result = intcode::run_program(program, vec![seq[i], input]).ensure_terminated();
        input = result.output[0];
    }

    return input;
}

fn run_program_for_part2(program: &intcode::Program, seq: Vec<i64>) -> i64 {
    let mut amplifiers = seq
        .iter()
        .map(|_| None)
        .collect::<Vec<Option<intcode::ProgramResult>>>();

    loop {
        for i in 0..seq.len() {
            let current_state = amplifiers[i].as_ref();
            match (i, current_state) {
                (0, None) => {
                    amplifiers[i] = Some(intcode::run_program(program, vec![seq[i], 0]));
                }
                (i, None) => {
                    let prev_amplifier = amplifiers[(i - 1) % amplifiers.len()].as_ref().unwrap();
                    let mut input = vec![seq[i]];
                    input.extend(prev_amplifier.get_output());
                    amplifiers[i] = Some(intcode::run_program(program, input));
                }
                (_, Some(intcode::ProgramResult::WaitingForInput(p))) => {
                    let prev_amplifier = amplifiers[(i + seq.len() - 1) % amplifiers.len()]
                        .as_ref()
                        .unwrap();
                    amplifiers[i] = Some(p.resume(prev_amplifier.get_output().to_vec()))
                }
                _ => (),
            }
        }

        match amplifiers.last() {
            Some(Some(intcode::ProgramResult::Terminated(t))) => return *t.output.first().unwrap(),
            _ => (),
        }
    }
}

impl Day for Day7 {
    type Intermediate = intcode::Program;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        intcode::read_program(file)
    }

    fn part1(program: &Self::Intermediate) -> String {
        (0..5)
            .permutations(5)
            .map(|input_seq| run_program_for_part1(program, input_seq))
            .max()
            .unwrap()
            .to_string()
    }

    fn part2(program: &Self::Intermediate) -> String {
        (5..10)
            .permutations(5)
            .map(|input_seq| run_program_for_part2(program, input_seq))
            .max()
            .unwrap()
            .to_string()
    }
}
