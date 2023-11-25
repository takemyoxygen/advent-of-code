use std::collections::HashSet;

use itertools::Itertools;

use crate::{solver::Day, utils};

pub struct Day24;

const SIZE: usize = 5;

const DELTAS: [[i32; 2]; 4] = [[-1, 0], [1, 0], [0, 1], [0, -1]];

type LevelPos = (i32, usize, usize); // (level, x, y)
type LevelState = HashSet<LevelPos>; // positions of bugs

fn adj(x: usize, y: usize) -> Vec<(usize, usize)> {
    DELTAS
        .iter()
        .map(|[dx, dy]| (x as i32 + dx, y as i32 + dy))
        .filter(|(x, y)| y >= &0 && x >= &0)
        .map(|(x, y)| (x as usize, y as usize))
        .filter(|(x, y)| y < &SIZE && x < &SIZE)
        .collect()
}

fn adj2((level, x, y): LevelPos) -> HashSet<LevelPos> {
    // Start with adjacent cells on the same level (excluding (2, 2) though)
    let mut result: HashSet<LevelPos> = adj(x, y)
        .into_iter()
        .filter(|pos| pos != &(2, 2))
        .map(|(x, y)| (level, x, y))
        .collect();

    // If adjacent to (2, 2) - add entire edge of the inner level as adjacent
    match (x, y) {
        (2, 1) => {
            for inner_x in 0..SIZE {
                result.insert((level + 1, inner_x, 0));
            }
        }
        (2, 3) => {
            for inner_x in 0..SIZE {
                result.insert((level + 1, inner_x, SIZE - 1));
            }
        }
        (1, 2) => {
            for inner_y in 0..SIZE {
                result.insert((level + 1, 0, inner_y));
            }
        }
        (3, 2) => {
            for inner_y in 0..SIZE {
                result.insert((level + 1, SIZE - 1, inner_y));
            }
        }
        _ => {}
    }

    // If on edge - add cells from the outer level as adjacent
    if y == 0 {
        result.insert((level - 1, 2, 1));
    } else if y == SIZE - 1 {
        result.insert((level - 1, 2, 3));
    }

    if x == 0 {
        result.insert((level - 1, 1, 2));
    } else if x == SIZE - 1 {
        result.insert((level - 1, 3, 2));
    }

    return result;
}

fn run(state: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut result = state.clone();

    for y in 0..state.len() {
        for x in 0..state[y].len() {
            let current = state[y][x];
            let adj_bugs = adj(x, y)
                .iter()
                .filter(|(ax, ay)| state[*ay][*ax] == '#')
                .count();

            result[y][x] = match (current, adj_bugs) {
                ('#', 1) => '#',
                ('#', _) => '.',
                ('.', 1 | 2) => '#',
                _ => '.',
            };
        }
    }

    result
}

fn run2(state: &LevelState) -> LevelState {
    let affected_levels = state
        .iter()
        .flat_map(|(l, _, _)| [l - 1, *l, l + 1])
        .collect::<HashSet<_>>();

    let mut next = LevelState::new();

    for level in affected_levels {
        for x in 0..SIZE {
            for y in 0..SIZE {
                if (x, y) == (2, 2) {
                    continue;
                }

                let adj_bugs = adj2((level, x, y))
                    .iter()
                    .filter(|pos| state.contains(pos))
                    .count();

                if (state.contains(&(level, x, y)) && adj_bugs == 1)
                    || !state.contains(&(level, x, y)) && (adj_bugs == 1 || adj_bugs == 2)
                {
                    next.insert((level, x, y));
                }
            }
        }
    }

    next
}

fn state_to_str(state: &Vec<Vec<char>>) -> String {
    state
        .iter()
        .map(|line| line.into_iter().collect::<String>())
        .join("\n")
}

fn biodiversity(state: &Vec<Vec<char>>) -> u128 {
    let mut result = 0;
    for y in 0..state.len() {
        for x in 0..state[y].len() {
            if state[y][x] == '#' {
                let power = y * state[x].len() + x;
                result += (2 as u128).pow(power as u32)
            }
        }
    }

    result
}

impl Day for Day24 {
    type Intermediate = Vec<Vec<char>>;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        utils::read_lines(file)
            .map(|line| line.chars().collect())
            .collect()
    }

    fn part1(input: &Self::Intermediate) -> String {
        let mut history = HashSet::new();
        let mut state = input.clone();
        loop {
            let state_hash = state_to_str(&state);
            if !history.insert(state_hash.clone()) {
                return biodiversity(&state).to_string();
            }
            state = run(&state);
        }
    }

    fn part2(input: &Self::Intermediate) -> String {
        let mut level_state = LevelState::new();
        for y in 0..input.len() {
            for x in 0..input[y].len() {
                if input[y][x] == '#' {
                    level_state.insert((0, x, y));
                }
            }
        }

        for _ in 0..200 {
            level_state = run2(&level_state);
        }

        level_state.len().to_string()
    }
}
