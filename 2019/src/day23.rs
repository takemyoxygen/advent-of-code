use itertools::Itertools;

use crate::{intcode, solver::Day};

struct Computer {
    address: i64,
    program: intcode::Program,
    program_state: Option<intcode::ProgramResult>,
    output: Vec<i64>,
    input: Vec<i64>,
    idle_counter: usize,
}

impl Computer {
    fn new(program: intcode::Program, address: i64) -> Computer {
        Computer {
            address: address,
            program: program,
            program_state: None,
            output: vec![],
            input: vec![],
            idle_counter: 0,
        }
    }

    fn consume_output(&mut self) -> Vec<Vec<i64>> {
        let output = self.output.chunks(3).map(|x| x.to_vec()).collect();
        self.output.clear();
        return output;
    }

    fn enqueue_input(&mut self, x: i64, y: i64) {
        self.input.push(x);
        self.input.push(y);
    }

    fn run(&mut self) {
        let current_state = &self.program_state;
        let new_state;
        match current_state {
            None => new_state = intcode::run_program(&self.program, vec![self.address]),
            Some(intcode::ProgramResult::Terminated(_)) => {
                panic!("Computer is in terminated state!")
            }
            Some(intcode::ProgramResult::WaitingForInput(prog)) => {
                let input = if self.input.is_empty() {
                    self.idle_counter += 1;
                    vec![-1]
                } else {
                    self.idle_counter = 0;
                    self.input.clone()
                };
                new_state = prog.resume(input)
            }
        }

        self.output.extend(new_state.get_output());
        self.program_state = Some(new_state);
        self.input.clear();
    }
}

pub struct Day23;

impl Day for Day23 {
    type Intermediate = intcode::Program;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        intcode::read_program(file)
    }

    fn part1(input: &Self::Intermediate) -> String {
        let mut computers = (0..50)
            .map(|addr| Computer::new(input.clone(), addr))
            .collect_vec();

        loop {
            for i in 0..computers.len() {
                computers[i].run();
            }

            for i in 0..computers.len() {
                let output = computers[i].consume_output();
                for out in output {
                    match out[..] {
                        [255, _, y] => return y.to_string(),
                        [dest, x, y] => computers[dest as usize].enqueue_input(x, y),
                        _ => panic!("Unexpedted output: {:?}", out),
                    }
                }
            }
        }
    }

    fn part2(input: &Self::Intermediate) -> String {
        let mut computers = (0..50)
            .map(|addr| Computer::new(input.clone(), addr))
            .collect_vec();

        let mut prev_nat = None;
        let mut nat = None;

        loop {
            for i in 0..computers.len() {
                computers[i].run();
            }

            for i in 0..computers.len() {
                let output = computers[i].consume_output();
                for out in output {
                    match out[..] {
                        [255, x, y] => nat = Some((x, y)),
                        [dest, x, y] => computers[dest as usize].enqueue_input(x, y),
                        _ => panic!("Unexpedted output: {:?}", out),
                    }
                }
            }

            if computers
                .iter()
                .all(|c| c.input.is_empty() && c.idle_counter > 0)
            {
                match nat {
                    Some((x, y)) => match prev_nat {
                        Some((_, prev_y)) if prev_y == y => return y.to_string(),
                        _ => {
                            prev_nat = nat;
                            computers[0].enqueue_input(x, y);
                        }
                    },
                    None => panic!("NAT is expected to have some values"),
                }
            }
        }
    }
}
