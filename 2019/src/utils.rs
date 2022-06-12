use std::path::Path;
use std::fs::File;
use std::io::{self, BufRead, Result};

pub fn read_lines(file: &Path) -> impl Iterator<Item = Result<String>> {
    let file = File::open(file).unwrap();
    io::BufReader::new(file).lines()
}
