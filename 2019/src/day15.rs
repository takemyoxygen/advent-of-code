use std::collections::{HashMap, HashSet, VecDeque};

use crate::{
    coord::{self, VecXD},
    intcode,
    solver::Day,
};

pub struct Day15;

const MOVEMENTS: [(i64, coord::Vec2D); 4] = [
    (1, coord::Vec2D { x: 0, y: -1 }),
    (2, coord::Vec2D { x: 0, y: 1 }),
    (3, coord::Vec2D { x: -1, y: 0 }),
    (4, coord::Vec2D { x: 1, y: 0 }),
];

#[derive(PartialEq)]
pub enum Location {
    Free,
    Wall,
    Oxygen,
}

fn move_robot(
    direction: i64,
    program: &intcode::WaitingForInputProgram,
) -> (i64, intcode::WaitingForInputProgram) {
    match program.resume(vec![direction]) {
        intcode::ProgramResult::WaitingForInput(next) => (next.output[0], next),
        intcode::ProgramResult::Terminated(_) => panic!("Program terminated"),
    }
}

fn init_robot(program: &intcode::Program) -> intcode::WaitingForInputProgram {
    match intcode::run_program(program, vec![]) {
        intcode::ProgramResult::WaitingForInput(next) => next,
        intcode::ProgramResult::Terminated(_) => panic!("Program terminated"),
    }
}

impl Day for Day15 {
    type Intermediate = (HashMap<coord::Vec2D, Location>, coord::Vec2D, usize);

    fn process(file: &std::path::Path) -> Self::Intermediate {
        let program = &intcode::read_program(file);
        let robot = init_robot(program);
        let mut queue = VecDeque::from([(coord::Vec2D::zero(), robot, 0)]);
        let mut map = HashMap::from([(coord::Vec2D::zero(), Location::Free)]);

        let mut oxygen_location = None;
        let mut moves_to_oxygen = None;

        while !queue.is_empty() {
            let (pos, robot, moves) = queue.pop_front().unwrap();

            for (dir, step) in MOVEMENTS {
                let (output, next_robot) = move_robot(dir, &robot);
                let next_pos = pos.add(&step);

                if map.contains_key(&next_pos) {
                    continue;
                }

                match output {
                    0 => {
                        map.insert(next_pos, Location::Wall);
                    }
                    1 => {
                        map.insert(next_pos, Location::Free);
                        queue.push_back((next_pos, next_robot, moves + 1));
                    }
                    2 => {
                        map.insert(next_pos, Location::Oxygen);
                        oxygen_location = Some(next_pos);
                        moves_to_oxygen = Some(moves + 1);
                        queue.push_back((next_pos, next_robot, moves + 1));
                    }
                    _ => panic!("Unexpected output: {}", output),
                }
            }
        }

        (map, oxygen_location.unwrap(), moves_to_oxygen.unwrap())
    }

    fn part1((_, _, moves): &Self::Intermediate) -> String {
        moves.to_string()
    }

    fn part2((map, start, _): &Self::Intermediate) -> String {
        let mut minutes = 0;
        let free_cells_count = map.values().filter(|&l| *l == Location::Free).count();
        let mut oxygen_cells = HashSet::from([start.clone()]);
        let mut prev_oxygen = vec![start.clone()];

        while oxygen_cells.len() != free_cells_count + 1 {
            let next_oxygen = prev_oxygen
                .iter()
                .flat_map(|oxy| MOVEMENTS.map(|(_, step)| oxy.add(&step)))
                .filter(|p| map.get(p) == Some(&Location::Free) && !oxygen_cells.contains(p))
                .collect::<Vec<coord::Vec2D>>();

            for p in next_oxygen.iter() {
                oxygen_cells.insert(p.clone());
            }

            prev_oxygen = next_oxygen;

            minutes += 1;
        }

        minutes.to_string()
    }
}
