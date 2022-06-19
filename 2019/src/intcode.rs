use std::path::Path;

use crate::utils::read_lines;

pub type Program = Vec<i32>;

#[derive(Debug)]
pub struct TerminatedProgram {
    pub output: Vec<i32>,
    pub program: Program,
}

#[derive(Debug)]
pub struct WaitingForInputProgram {
    program: Program,
    pos: usize,
    pub output: Vec<i32>,
}

#[derive(Debug)]
pub enum ProgramResult {
    Terminated(TerminatedProgram),
    WaitingForInput(WaitingForInputProgram),
}

impl ProgramResult {
    pub fn ensure_terminated(self) -> TerminatedProgram {
        match self {
            ProgramResult::Terminated(res) => res,
            _ => panic!("Program is not terminated"),
        }
    }

    pub fn get_output(&self) -> &Vec<i32> {
        match self {
            ProgramResult::Terminated(t) => &t.output,
            ProgramResult::WaitingForInput(w) => &w.output,
        }
    }
}

impl WaitingForInputProgram {
    pub fn resume(&self, input: Vec<i32>) -> ProgramResult {
        return run_program_internal(&self.program, self.pos, input);
    }
}

fn eval_argument(program: &Program, arg_value: i32, arg_index: usize, instruction: i32) -> i32 {
    let is_immediate = (instruction / i32::pow(10, (arg_index as u32) + 1)) % 10 == 1;
    if is_immediate {
        arg_value
    } else {
        program[arg_value as usize]
    }
}

pub fn read_program(file: &Path) -> Program {
    let line = read_lines(file).next().unwrap();
    line.split(",")
        .map(|token| str::parse::<i32>(token).unwrap())
        .collect()
}

fn run_program_internal(
    source_program: &Program,
    source_position: usize,
    input: Vec<i32>,
) -> ProgramResult {
    let mut program = source_program.clone();
    let mut pos = source_position;
    let mut output = Vec::new();
    let mut input_iter = input.iter();

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
                match input_iter.next() {
                    Some(i) => {
                        program[dst] = *i;
                        pos += 2;
                    }
                    None => {
                        return ProgramResult::WaitingForInput(WaitingForInputProgram {
                            pos: pos,
                            program: program,
                            output: output,
                        })
                    }
                }
            }
            4 => {
                output.push(eval_argument(&program, program[pos + 1], 1, instruction));
                pos += 2;
            }
            5 => {
                if eval_argument(&program, program[pos + 1], 1, instruction) != 0 {
                    pos = eval_argument(&program, program[pos + 2], 2, instruction) as usize
                } else {
                    pos += 3;
                }
            }
            6 => {
                if eval_argument(&program, program[pos + 1], 1, instruction) == 0 {
                    pos = eval_argument(&program, program[pos + 2], 2, instruction) as usize
                } else {
                    pos += 3
                }
            }
            7 => {
                let dst = program[pos + 3] as usize;
                program[dst] = if eval_argument(&program, program[pos + 1], 1, instruction)
                    < eval_argument(&program, program[pos + 2], 2, instruction)
                {
                    1
                } else {
                    0
                };
                pos += 4
            }
            8 => {
                let dst = program[pos + 3] as usize;
                program[dst] = if eval_argument(&program, program[pos + 1], 1, instruction)
                    == eval_argument(&program, program[pos + 2], 2, instruction)
                {
                    1
                } else {
                    0
                };
                pos += 4
            }
            99 => {
                break;
            }
            _ => panic!("Unknown instruction {}", instruction),
        }
    }

    ProgramResult::Terminated(TerminatedProgram {
        output: output,
        program: program,
    })
}

pub fn run_program(source_program: &Program, input: Vec<i32>) -> ProgramResult {
    return run_program_internal(source_program, 0, input);
}
