use crate::{solver::Day, utils::read_lines};

pub struct Day5;

fn eval_argument(program: &Vec<i32>, arg_value: i32, arg_index: usize, instruction: i32) -> i32 {
    let is_immediate = (instruction / i32::pow(10, (arg_index as u32) + 1)) % 10 == 1;
    if is_immediate {
        arg_value
    } else {
        program[arg_value as usize]
    }
}

fn run_program(
    original_program: &Vec<i32>,
    input: i32,
    extra_instructions: fn(&mut Vec<i32>, i32, usize) -> usize,
) -> Vec<i32> {
    let mut program = original_program.clone();
    let mut pos = 0;
    let mut output = Vec::new();

    loop {
        let instruction = program[pos];
        match instruction % 100 {
            1 => {
                let dst = program[pos + 3] as usize;
                program[dst] = eval_argument(&program, program[pos + 1], 1, instruction)
                    + eval_argument(&program, program[pos + 2], 2, instruction);
                pos += 4
            }
            2 => {
                let dst = program[pos + 3] as usize;
                program[dst] = eval_argument(&program, program[pos + 1], 1, instruction)
                    * eval_argument(&program, program[pos + 2], 2, instruction);
                pos += 4
            }
            3 => {
                let dst = program[pos + 1] as usize;
                program[dst] = input;
                pos += 2;
            }
            4 => {
                output.push(eval_argument(&program, program[pos + 1], 1, instruction));
                pos += 2;
            }
            99 => {
                break;
            }
            _ => {
                pos = extra_instructions(&mut program, instruction, pos);
            }
        }
    }

    output
}

fn jump_instructions(program: &mut Vec<i32>, instruction: i32, pos: usize) -> usize {
    match instruction % 100 {
        5 => {
            if eval_argument(program, program[pos + 1], 1, instruction) != 0 {
                eval_argument(program, program[pos + 2], 2, instruction) as usize
            } else {
                pos + 3
            }
        }
        6 => {
            if eval_argument(program, program[pos + 1], 1, instruction) == 0 {
                eval_argument(program, program[pos + 2], 2, instruction) as usize
            } else {
                pos + 3
            }
        }
        7 => {
            let dst = program[pos + 3] as usize;
            program[dst] = if eval_argument(program, program[pos + 1], 1, instruction)
                < eval_argument(program, program[pos + 2], 2, instruction)
            {
                1
            } else {
                0
            };
            pos + 4
        }
        8 => {
            let dst = program[pos + 3] as usize;
            program[dst] = if eval_argument(program, program[pos + 1], 1, instruction)
                == eval_argument(program, program[pos + 2], 2, instruction)
            {
                1
            } else {
                0
            };
            pos + 4
        }
        _ => panic!("Unknown instruction {}", instruction),
    }
}

impl Day for Day5 {
    type Intermediate = Vec<i32>;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        let line = read_lines(file).next().unwrap();
        line.split(",")
            .map(|token| str::parse::<i32>(token).unwrap())
            .collect()
    }

    fn part1(program: &Self::Intermediate) -> String {
        let output = run_program(&program, 1, |_, _, pos| pos);
        output
            .iter()
            .map(|i| i.to_string())
            .collect::<Vec<String>>()
            .join("")
    }

    fn part2(program: &Self::Intermediate) -> String {
        let output = run_program(&program, 5, jump_instructions);
        output
            .iter()
            .map(|i| i.to_string())
            .collect::<Vec<String>>()
            .join("")
    }
}
