use std::{
    collections::{HashMap, HashSet},
    vec,
};

use crate::{coord, intcode, solver::Day};

pub type Colored = HashMap<coord::Vec2D, i64>;

pub struct Day11;

fn run_robot(start: &coord::Vec2D, init_colors: &Colored, program: &intcode::Program) -> Colored {
    let mut pos = start.clone();
    let mut dir = &coord::Vec2D { x: 0, y: 1 };
    let mut colors = init_colors.clone();
    let mut program_state = intcode::run_program(program, vec![]);
    loop {
        let output = program_state.get_output();
        match output[..] {
            [] => (),
            [color_mark, dir_mark] => {
                colors.insert(pos.clone(), color_mark);
                dir = if dir_mark == 0 {
                    coord::left(dir)
                } else {
                    coord::right(dir)
                };
                pos = coord::sum(&pos, dir);
            }
            _ => panic!(
                "Robot should output exactly 2 numbers. Actual output: {:?}",
                output
            ),
        }

        match program_state {
            intcode::ProgramResult::WaitingForInput(waiting) => {
                let current_color = colors.get(&pos).unwrap_or(&0);
                program_state = waiting.resume(vec![*current_color]);
            }
            intcode::ProgramResult::Terminated(_) => return colors,
        }
    }
}

fn format_colored(colored: &Colored) -> String {
    let whites = colored
        .iter()
        .filter(|&(_, &color)| color == 1)
        .map(|(pos, _)| pos)
        .collect::<HashSet<&coord::Vec2D>>();

    let min_x = whites.iter().map(|p| p.x).min().unwrap();
    let max_x = whites.iter().map(|p| p.x).max().unwrap();
    let min_y = whites.iter().map(|p| p.y).min().unwrap();
    let max_y = whites.iter().map(|p| p.y).max().unwrap();

    let mut output = String::new();
    for y in (min_y..=max_y).rev() {
        for x in min_x..=max_x {
            let char = if whites.contains(&coord::Vec2D { x, y }) {
                '#'
            } else {
                ' '
            };
            output.push(char);
        }
        output.push('\n')
    }

    output
}

impl Day for Day11 {
    type Intermediate = intcode::Program;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        intcode::read_program(file)
    }

    fn part1(program: &Self::Intermediate) -> String {
        run_robot(&coord::zero(), &HashMap::new(), program)
            .len()
            .to_string()
    }

    fn part2(program: &Self::Intermediate) -> String {
        let start = coord::zero();
        let mut init_colors = HashMap::new();
        init_colors.insert(start.clone(), 1);
        let colored = run_robot(&start, &init_colors, program);
        return format!("\n{}", format_colored(&colored));
    }
}
