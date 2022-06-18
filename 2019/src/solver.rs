use std::fmt;
use std::path::Path;

pub struct Answer {
    pub part1: Option<String>,
    pub part2: Option<String>,
}

impl std::fmt::Display for Answer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let empty = String::new();
        let p1 = self.part1.as_ref().or(Some(&empty)).unwrap();
        let p2 = self.part2.as_ref().or(Some(&empty)).unwrap();
        write!(f, "Part1: {}\nPart2: {}", p1, p2)
    }
}

pub trait Day {
    type Intermediate;

    fn process(file: &Path) -> Self::Intermediate;
    fn part1(input: &Self::Intermediate) -> String;
    fn part2(input: &Self::Intermediate) -> String;
}

pub trait Solvable {
    fn solve(&self, file: &Path, part1: bool, part2: bool) -> Answer;
}

impl<T: Day> Solvable for T {
    fn solve(&self, file: &Path, part1: bool, part2: bool) -> Answer {
        let input = Self::process(file);
        let part1 = if part1 {
            Self::part1(&input)
        } else {
            String::from("")
        };
        let part2 = if part2 {
            Self::part2(&input)
        } else {
            String::from("")
        };

        Answer {
            part1: Some(part1),
            part2: Some(part2),
        }
    }
}
