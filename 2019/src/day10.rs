use std::{collections::HashSet, f32::consts::PI};

use im::HashMap;

use crate::{solver::Day, utils::read_lines};

pub struct Day10;

type Position = (usize, usize);

fn dist((x1, y1): &Position, (x2, y2): &Position) -> usize {
    x1.abs_diff(*x2) + y1.abs_diff(*y2)
}

fn get_angle((ax, ay): Position, (bx, by): Position) -> i32 {
    let dx = (bx as i32) - (ax as i32);
    let dy: i32 = (by as i32) - (ay as i32);
    let angle = match (dx, dy) {
        (0, _) => 0.0,
        (_, 0) => PI / 2.0,
        _ => ((dx as f32).abs() / (dy as f32).abs()).atan(),
    };
    let adjusted_andle = match (dx.signum(), dy.signum()) {
        (0 | 1, -1 | 0) => angle,
        (0 | 1, 1) => PI - angle,
        (-1, 1 | 0) => PI + angle,
        (-1, -1) => 2.0 * PI - angle,
        _ => panic!("Unexpected dx = {}, dy = {}", dx, dy),
    };
    return (adjusted_andle * 10000000.0).round() as i32;
}

fn get_best_position(asteroids: &HashSet<Position>) -> (Position, usize) {
    let mut best_visible = 0;
    let mut best_location = (0, 0);
    for ast in asteroids.iter() {
        let mut others = asteroids.clone();
        others.remove(ast);
        let mut visible = HashSet::new();
        for other in others {
            visible.insert(get_angle(*ast, other));
        }
        if visible.len() > best_visible {
            best_visible = visible.len();
            best_location = *ast;
        }
    }

    (best_location, best_visible)
}

impl Day for Day10 {
    type Intermediate = (HashSet<Position>, Position, usize);

    fn process(file: &std::path::Path) -> Self::Intermediate {
        let mut asteroids = HashSet::new();
        let mut y = 0;
        let lines = read_lines(file).collect::<Vec<String>>();
        for line in lines.iter() {
            for (x, _) in line.chars().enumerate().filter(|(_, c)| c == &'#') {
                asteroids.insert((x, y));
            }
            y += 1;
        }

        let (best_pos, best_visible) = get_best_position(&asteroids);
        return (asteroids, best_pos, best_visible);
    }

    fn part1((_, _, best_visible): &Self::Intermediate) -> String {
        best_visible.to_string()
    }

    fn part2((asteroids, base, _): &Self::Intermediate) -> String {
        let mut others = asteroids.clone();
        others.remove(&base);

        let mut by_angle: HashMap<i32, Vec<Position>> = HashMap::new();

        for other in others {
            let angle = get_angle(*base, other);
            by_angle
                .entry(angle)
                .and_modify(|v| v.push(other))
                .or_insert(vec![other]);
        }

        let mut angles: Vec<i32> = by_angle.keys().cloned().collect();
        angles.sort();

        for a in angles.iter() {
            by_angle
                .get_mut(&a)
                .unwrap()
                .sort_by_key(|other| dist(&base, other));
        }

        let mut vaporised = Vec::new();

        let target = 200;

        loop {
            for a in angles.iter() {
                if vaporised.len() == target {
                    let (x, y) = vaporised[target - 1];
                    return ((x * 100 + y) as usize).to_string();
                }
                match by_angle.get_mut(&a) {
                    Some(group) => {
                        vaporised.push(group.remove(0));
                    }
                    None => (),
                }
            }
        }
    }
}
