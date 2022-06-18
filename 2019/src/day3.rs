use crate::solver::Day;
use crate::utils::read_lines;
use core::cmp::min;
use std::collections::HashMap;

pub struct Day3;

type Point = (i32, i32);
type Move = (char, i32);
type Wire = Vec<Move>;

fn parse_move(s: &str) -> Move {
    let dir = &s[..1];
    let count = &s[1..];
    (
        dir.chars().next().unwrap(),
        str::parse::<i32>(count).unwrap(),
    )
}

fn move_dir(start: Point, mv: &Move) -> Vec<Point> {
    let (x, y) = start;
    let (dir, steps) = *mv;
    return (1..=steps)
        .map(|i| match dir {
            'R' => (x + i, y),
            'L' => (x - i, y),
            'D' => (x, y - i),
            'U' => (x, y + i),
            _ => panic!("Unknown direction"),
        })
        .collect();
}

fn get_wire_points(wire: &Wire) -> Vec<Point> {
    let mut points: Vec<Point> = vec![(0, 0)];
    for mv in wire.iter() {
        let start = points.last().copied().unwrap();
        points.append(&mut move_dir(start, mv));
    }
    points
}

impl Day for Day3 {
    type Intermediate = Vec<Wire>;
    fn process(file: &std::path::Path) -> Self::Intermediate {
        let lines = read_lines(file);
        return lines
            .map(|line| line.split(',').map(|mv| parse_move(mv)).collect())
            .collect();
    }

    fn part1(wires: &Self::Intermediate) -> String {
        let mut all_wire_points = HashMap::new();
        let mut closest = std::i32::MAX;
        for (i, wire) in wires.iter().enumerate() {
            for pos in get_wire_points(wire) {
                match (pos, all_wire_points.insert(pos, i)) {
                    ((0, 0), _) => (),
                    ((x, y), Some(prev_i)) if prev_i != i => {
                        closest = min(closest, i32::abs(x) + i32::abs(y))
                    }
                    _ => (),
                }
            }
        }

        closest.to_string()
    }
    fn part2(wires: &Self::Intermediate) -> std::string::String {
        let mut all_wire_points = HashMap::new();
        let mut smallest_delay = std::usize::MAX;

        for (i, wire) in wires.iter().enumerate() {
            for (steps, pos) in get_wire_points(wire).into_iter().enumerate() {
                match (pos, all_wire_points.get(&pos)) {
                    ((0, 0), _) => (),
                    (_, Some((prev_steps, prev_i))) if *prev_i != i => {
                        smallest_delay = min(smallest_delay, prev_steps + steps);
                    }
                    _ => {
                        all_wire_points.insert(pos, (steps, i));
                    }
                }
            }
        }

        smallest_delay.to_string()
    }
}
