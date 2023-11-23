use itertools::Itertools;

use crate::solver::Day;
use crate::utils;

pub struct Day22;

#[derive(Debug)]
pub enum Operation {
    DealNewStack,
    Cut(i32),
    DealWithIncr(usize),
}

fn prev_pos(final_position: i128, size: i128, operations: &Vec<&Operation>) -> i128 {
    let mut pos = final_position;
    let reversed = operations.iter().rev().collect::<Vec<_>>();

    for op in reversed.iter() {
        match op {
            Operation::DealNewStack => pos = size - 1 - pos,
            Operation::Cut(n) => {
                pos = ((pos as i128) + (*n as i128)).rem_euclid(size as i128) as i128
            }
            Operation::DealWithIncr(n) => {
                pos = ((modinverse::modinverse(*n as i128, size as i128).unwrap() as i128) * pos)
                    .rem_euclid(size) as i128;
            }
        }
    }

    pos
}

fn next_pos(start_position: i128, size: i128, operations: &Vec<Operation>) -> i128 {
    let mut pos = start_position;
    for op in operations {
        match op {
            Operation::DealNewStack => pos = size - 1 - pos,
            Operation::Cut(n) => {
                pos = ((pos as i128) - (*n as i128)).rem_euclid(size as i128) as i128
            }
            Operation::DealWithIncr(n) => pos = (pos * (*n as i128)).rem_euclid(size),
        }
    }
    pos
}

impl Day for Day22 {
    type Intermediate = Vec<Operation>;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        utils::read_lines(file)
            .map(|op| {
                if op == "deal into new stack" {
                    return Operation::DealNewStack;
                }

                let param = str::parse(op.split_whitespace().last().unwrap()).unwrap();

                if op.starts_with("cut ") {
                    return Operation::Cut(param);
                }

                return Operation::DealWithIncr(param as usize);
            })
            .collect()
    }

    fn part1(operations: &Self::Intermediate) -> String {
        next_pos(2019, 10007, operations).to_string()
    }

    fn part2(operations: &Self::Intermediate) -> String {
        //https://www.reddit.com/r/adventofcode/comments/ee0rqi/comment/fbnifwk/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
        let size: i128 = 119315717514047;
        let times: i128 = 101741582076661;
        let x: i128 = 2020;
        let ops_borrowed = operations.iter().map(|x| x).collect_vec();
        let y = prev_pos(x as i128, size, &ops_borrowed) as i128;
        let z = prev_pos(y as i128, size, &ops_borrowed) as i128;
        let a = ((y - z)
            * modinverse::modinverse(((x as i128) - y + (size as i128)) as i128, size as i128)
                .unwrap())
        .rem_euclid(size as i128);
        let b = (y - a * (x as i128)).rem_euclid(size as i128);

        let pw = mod_exp::mod_exp(a, times, size);
        let mi = modinverse::modinverse(a - 1, size).unwrap();

        (pw * x + (((pw - 1) * mi).rem_euclid(size) * b).rem_euclid(size))
            .rem_euclid(size)
            .to_string()
    }
}
