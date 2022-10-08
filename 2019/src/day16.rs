use std::iter::repeat;

use itertools::{repeat_n, Itertools};

use crate::{solver::Day, utils};

pub struct Day16;

const BASE_PATTERN: [i32; 4] = [0, 1, 0, -1];

struct Pattern {
    calc_pos: usize,
    pattern_offset: usize,
    pattern_elements_taken: usize,
}

// impl Iterator for Pattern {
//     type Item;

//     fn next(&mut self) -> Option<Self::Item> {
//         todo!()
//     }
// }

fn pattern<'a>(pos: &'a usize) -> impl Iterator<Item = i32> + 'a {
    let chunks = BASE_PATTERN.iter().flat_map(|&d| repeat_n(d, *pos + 1));
    repeat(chunks).flatten().skip(1)
}

fn fft(input: Vec<i32>) -> Vec<i32> {
    let mut output: Vec<i32> = Vec::with_capacity(input.len());

    for i in 0..input.len() {
        output.push(i32::abs(
            input
                .iter()
                .zip(pattern(&i))
                .map(|(&a, b)| a * b)
                .sum::<i32>()
                % 10,
        ));
    }

    output
}

impl Day for Day16 {
    type Intermediate = Vec<i32>;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        let line = utils::read_lines(file).next().unwrap();

        let chars = line.split("").collect::<Vec<_>>();

        (&chars[1..chars.len() - 1])
            .iter()
            .map(|d| str::parse::<i32>(d).unwrap())
            .collect()
    }

    fn part1(input: &Self::Intermediate) -> String {
        (1..=100)
            .fold(input.clone(), |acc, _| fft(acc))
            .iter()
            .take(8)
            .join("")
    }

    fn part2(input: &Self::Intermediate) -> String {
        todo!()
    }
}
