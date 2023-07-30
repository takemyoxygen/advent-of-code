use crate::{solver::Day, intcode};
use std::{cmp, collections::HashSet};

pub struct Day19;

fn run_program(program: &intcode::Program, x: usize, y: usize) -> i64 {
    intcode::run_program(program, vec![x as i64, y as i64]).get_output()[0]
}

fn is_pulling(program: &intcode::Program, x:usize, y:usize) -> bool {
    return run_program(program, x, y) == 1;
}

fn bounding_xs_at_y(program: &intcode::Program, prev: (usize, usize), y: usize) -> (usize, usize) {
    let (prev_x1, prev_x2) = prev;
    let x2 = if is_pulling(program, prev_x2 + 1, y) {prev_x2 + 1} else {prev_x2};
    let x1 = if is_pulling(program, prev_x1, y) {prev_x1} else {prev_x1 + 1};
    
    assert!(is_pulling(program, x1, y), "Should pull at x1");
    assert!(is_pulling(program, x2, y), "Should pull at x2");
    
    return (x1, x2);
}

fn find_start(program: &intcode::Program) -> (usize, usize) {
    for x in 1..10 {
        for y in 1..10 {
            if is_pulling(program, x, y) {
                return (x, y)
            }
        }
    }
    panic!("Start position not found");
}

impl Day for Day19 {
    type Intermediate = intcode::Program;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        intcode::read_program(file)
    }

    fn part1(program: &Self::Intermediate) -> String {
        let mut result = 2;
        let size = 50;
        let (start_x, start_y) = find_start(program);
        let mut prev = (start_x, start_y);
        for y in start_y + 1..size {
            let (x1, x2) = bounding_xs_at_y(program, prev, y);
            if x1 > size {
                break;
            }
            result += cmp::min(size, x2) - x1 + 1;
            prev = (x1, x2);
        }

        result.to_string()
    }

    fn part2(program: &Self::Intermediate) -> String {
        let start = find_start(program);
        let size = 100;
        let mut y = start.1 + 1;
        let mut upper_border: HashSet<(usize, usize)> = HashSet::from_iter(vec![start]);
        let mut prev = start;
        loop {
            let (x1, x2) = bounding_xs_at_y(program, prev, y);

            if y >= size && upper_border.contains(&(y - size + 1, x1 + size - 1)) {
                return (x1 * 10000 + (y - size + 1)).to_string();
            }
            
            upper_border.insert((y, x2));
            prev = (x1, x2);
            
            y += 1;
        }
    }
}

