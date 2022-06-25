use std::{collections::HashMap, path::Path, slice::Iter};

use crate::utils::read_lines;

pub type Program = Vec<i64>;
pub type Memory = HashMap<usize, i64>;

#[derive(Debug)]
pub struct TerminatedProgram {
    pub output: Vec<i64>,
    pub memory: Memory,
}

#[derive(Debug)]
pub struct WaitingForInputProgram {
    memory: Memory,
    relative_base: i64,
    pos: usize,
    pub output: Vec<i64>,
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

    pub fn get_output(&self) -> &Vec<i64> {
        match self {
            ProgramResult::Terminated(t) => &t.output,
            ProgramResult::WaitingForInput(w) => &w.output,
        }
    }
}

impl WaitingForInputProgram {
    pub fn resume(&self, input: Vec<i64>) -> ProgramResult {
        return run_program_internal(self.memory.clone(), self.pos, input, self.relative_base);
    }
}

fn get_val(memory: &Memory, pos: usize) -> i64 {
    return *memory.get(&pos).unwrap_or(&0);
}

pub fn read_program(file: &Path) -> Program {
    let line = read_lines(file).next().unwrap();
    line.split(",")
        .map(|token| str::parse::<i64>(token).unwrap())
        .collect()
}

struct ProgramState<'a> {
    position: usize,
    memory: Memory,
    input: Iter<'a, i64>,
    relative_base: i64,
    output: Vec<i64>,
}

fn read_instruction(state: &ProgramState) -> i64 {
    get_val(&state.memory, state.position)
}

fn parameter_mode(instruction: i64, index: usize) -> usize {
    ((instruction / i64::pow(10, (index as u32) + 1)) % 10) as usize
}

fn eval_arg(state: &ProgramState, index: usize, instruction: i64) -> i64 {
    let arg_value = get_val(&state.memory, state.position + index);
    match parameter_mode(instruction, index) {
        1 => arg_value,
        0 => *state.memory.get(&(arg_value as usize)).unwrap_or(&0),
        2 => *state
            .memory
            .get(&((arg_value + state.relative_base) as usize))
            .unwrap_or(&0),
        x => panic!("Unknown parameter type {}", x),
    }
}

fn eval_dst(state: &ProgramState, index: usize, instruction: i64) -> usize {
    let arg_value = get_val(&state.memory, state.position + index);
    match parameter_mode(instruction, index) {
        0 | 1 => arg_value as usize,
        2 => (arg_value + state.relative_base) as usize,
        x => panic!("Unknown parameter type {}", x),
    }
}

fn run_program_internal(
    memory: Memory,
    source_position: usize,
    input: Vec<i64>,
    source_relative_base: i64,
) -> ProgramResult {
    // let mut pos = source_position;
    // let mut relative_base = source_relative_base;
    // let mut output = Vec::new();
    // let mut input_iter = input.iter();
    // let mut working_memory = memory;

    let mut state = ProgramState {
        input: input.iter(),
        memory: memory,
        position: source_position,
        relative_base: source_relative_base,
        output: Vec::new(),
    };

    loop {
        let instruction = read_instruction(&state);
        match instruction % 100 {
            1 => {
                let dst = eval_dst(&state, 3, instruction);
                let new_val = eval_arg(&state, 1, instruction) + eval_arg(&state, 2, instruction);
                state.memory.insert(dst, new_val);
                state.position += 4;
            }
            2 => {
                let dst = eval_dst(&state, 3, instruction);
                let new_val = eval_arg(&state, 1, instruction) * eval_arg(&state, 2, instruction);
                state.memory.insert(dst, new_val);
                state.position += 4;
            }
            3 => {
                let dst = eval_dst(&state, 1, instruction);

                match state.input.next() {
                    Some(i) => {
                        state.memory.insert(dst, *i);
                        state.position += 2;
                    }
                    None => {
                        return ProgramResult::WaitingForInput(WaitingForInputProgram {
                            pos: state.position,
                            relative_base: state.relative_base,
                            memory: state.memory,
                            output: state.output,
                        })
                    }
                }
            }
            4 => {
                let out_val = eval_arg(&state, 1, instruction);
                state.output.push(out_val);
                state.position += 2;
            }
            5 => {
                let arg = eval_arg(&state, 1, instruction);
                if arg != 0 {
                    state.position = eval_arg(&state, 2, instruction) as usize;
                } else {
                    state.position += 3;
                }
            }
            6 => {
                let arg = eval_arg(&state, 1, instruction);
                if arg == 0 {
                    state.position = eval_arg(&state, 2, instruction) as usize;
                } else {
                    state.position += 3
                }
            }
            7 => {
                let dst = eval_dst(&state, 3, instruction);
                let arg1 = eval_arg(&state, 1, instruction);
                let arg2 = eval_arg(&state, 2, instruction);
                let new_val = if arg1 < arg2 { 1 } else { 0 };
                state.memory.insert(dst, new_val);
                state.position += 4
            }
            8 => {
                let dst = eval_dst(&state, 3, instruction);
                let arg1 = eval_arg(&state, 1, instruction);
                let arg2 = eval_arg(&state, 2, instruction);
                let new_val = if arg1 == arg2 { 1 } else { 0 };
                state.memory.insert(dst, new_val);
                state.position += 4
            }
            9 => {
                let offset = eval_arg(&state, 1, instruction);
                state.relative_base += offset;
                state.position += 2;
            }
            99 => {
                break;
            }
            _ => panic!("Unknown instruction {}", instruction),
        }
    }

    ProgramResult::Terminated(TerminatedProgram {
        output: state.output,
        memory: state.memory,
    })
}

pub fn run_program(source_program: &Program, input: Vec<i64>) -> ProgramResult {
    let memory = source_program
        .into_iter()
        .map(|x| *x)
        .enumerate()
        .collect::<Memory>();

    run_program_internal(memory, 0, input, 0)
}
