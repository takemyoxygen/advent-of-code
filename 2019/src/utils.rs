use std::cmp;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

pub fn read_lines(file: &Path) -> impl Iterator<Item = String> {
    let file = File::open(file).unwrap();
    io::BufReader::new(file).lines().map(|line| line.unwrap())
}

pub fn greatest_common_divisor(a: usize, b: usize) -> usize {
    let mut min = cmp::min(a, b);
    let mut max = cmp::max(a, b);

    loop {
        let res = max % min;
        if res == 0 {
            return min;
        }

        max = min;
        min = res;
    }
}

pub fn least_common_multiple_2(a: usize, b: usize) -> usize {
    let gcd = greatest_common_divisor(a, b);
    a / gcd * b
}

pub fn least_common_multiple_n<T: Iterator<Item = usize>>(xs: T) -> Option<usize> {
    xs.reduce(|acc, x| least_common_multiple_2(acc, x))
}
