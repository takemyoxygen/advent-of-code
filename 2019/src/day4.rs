pub struct Day4;

use crate::solver::Day;

fn gte(v1: &im::Vector<u8>, v2: &im::Vector<u8>) -> bool {
    for (x1, x2) in v1.iter().zip(v2.iter()) {
        if x1 > x2 {
            return true;
        } else if x2 > x1 {
            return false;
        }
    }

    true
}

fn count_passwords_part1(
    n: &im::Vector<u8>,
    has_reps: bool,
    lo: &im::Vector<u8>,
    hi: &im::Vector<u8>,
) -> u32 {
    if n.len() == 6 {
        return if has_reps && gte(n, lo) && gte(hi, n) {
            1
        } else {
            0
        };
    }

    let mut x = n.clone();
    let last_digit = n[n.len() - 1];
    x.push_back(last_digit);

    let mut count = count_passwords_part1(&x, true, lo, hi);

    for i in last_digit + 1..=9 {
        let mut y = n.clone();
        y.push_back(i);
        count += count_passwords_part1(&y, has_reps, lo, hi);
    }

    return count;
}

fn count_passwords_part2(
    n: &im::Vector<u8>,
    pair_start: Option<usize>,
    lo: &im::Vector<u8>,
    hi: &im::Vector<u8>,
) -> u32 {
    if n.len() == 6 {
        return if pair_start.is_some() && gte(n, lo) && gte(hi, n) {
            1
        } else {
            0
        };
    }

    let last_digit = n[n.len() - 1];

    let mut x = n.clone();
    x.push_back(last_digit);

    let pair_with_same = match pair_start {
        None if n.len() > 1 && n[n.len() - 2] == last_digit => None,
        None => Some(n.len() - 1),
        Some(p) if n.len() - p == 2 => None,
        i => i,
    };

    let mut count = count_passwords_part2(&x, pair_with_same, lo, hi);

    for i in last_digit + 1..=9 {
        let mut y = n.clone();
        y.push_back(i);
        count += count_passwords_part2(&y, pair_start, lo, hi);
    }

    return count;
}

impl Day for Day4 {
    type Intermediate = (im::Vector<u8>, im::Vector<u8>);

    fn process(_: &std::path::Path) -> Self::Intermediate {
        (im::vector![1, 5, 6, 2, 1, 8], im::vector![6, 5, 2, 5, 2, 7])
    }

    fn part1(input: &Self::Intermediate) -> std::string::String {
        let (lo, hi) = input;
        (lo[0]..=hi[0])
            .map(|start| count_passwords_part1(&im::vector![start], false, lo, hi))
            .sum::<u32>()
            .to_string()
    }

    fn part2(input: &Self::Intermediate) -> std::string::String {
        let (lo, hi) = input;
        (lo[0]..=hi[0])
            .map(|start| count_passwords_part2(&im::vector![start], None, lo, hi))
            .sum::<u32>()
            .to_string()
    }
}
