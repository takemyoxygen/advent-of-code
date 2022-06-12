use std::path::Path;
use std::fs::File;
use std::io::{self, BufRead};

pub fn read_lines(file: &Path) -> impl Iterator<Item = String> {
    let file = File::open(file).unwrap();
    io::BufReader::new(file).lines().map(|line| line.unwrap())
}
