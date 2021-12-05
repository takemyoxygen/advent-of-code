use std::io;
use std::io::Read;
use std::str::FromStr;

#[derive(Debug, Eq, PartialEq)]
enum Action {
    TurnOn,
    TurnOff,
    Toggle
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum OneOffLight {
    On,
    Off
}

type BrightLight = usize;

trait Light {
    fn apply(&self, action: &Action) -> Self;
}

impl Light for OneOffLight {

    fn apply(&self, action: &Action) -> OneOffLight {
        match *action {
            Action::TurnOn => OneOffLight::On,
            Action::TurnOff => OneOffLight::Off,
            Action::Toggle => match *self {
                OneOffLight::On => OneOffLight::Off,
                OneOffLight::Off => OneOffLight::On
            }
        }
    }
}

impl Light for BrightLight {
    fn apply(&self, action: &Action) -> BrightLight {
        match *action {
            Action::TurnOn => self + 1,
            Action::TurnOff if *self == 0 => 0,
            Action::TurnOff => self - 1,
            Action::Toggle => self + 2
         }
    }
}

type Coordinates = (usize, usize);

#[derive(Debug)]
struct Instruction {
    action: Action,
    start: Coordinates,
    end: Coordinates
}

type Grid<T> = Vec<Vec<T>>;

impl Instruction {

    fn parse_coordinates(string: &str) -> Coordinates {
        let tokens: Vec<_> = string.split(',').collect();
        let x = usize::from_str(tokens[0]).unwrap();
        let y = usize::from_str(tokens[1]).unwrap();
        (x, y)
    }

    fn parse(line: &str) -> Instruction {
        let tokens: Vec<_> = line.split_whitespace().collect();

        let action = match (tokens[0], tokens[1]) {
            ("toggle", _) => Action::Toggle,
            ("turn", "on") => Action::TurnOn,
            ("turn", "off") => Action::TurnOff,
            _ => panic!("Unexpected input: {}", line)
        };

        let (start, end) = if action == Action::Toggle { (1, 3) } else { (2, 4) };
        Instruction
            { action: action,
              start: Instruction::parse_coordinates(tokens[start]),
              end: Instruction::parse_coordinates(tokens[end]) }
    }

    fn apply_to<T: Light>(&self, grid: &mut Grid<T>) {
        for i in self.start.0..self.end.0 + 1 {
            for j in self.start.1..self.end.1 + 1 {
                grid[i][j] = grid[i][j].apply(&self.action);
            }
        }
    }
}

fn lights_count(grid: &Grid<OneOffLight>) -> usize {
    grid
        .into_iter()
        .flat_map(|x| x.into_iter())
        .filter(|&light| *light == OneOffLight::On)
        .count()
}

fn total_brightness(grid: &Grid<BrightLight>) -> usize {
    grid
        .into_iter()
        .flat_map(|x| x.into_iter())
        .fold(0, |acc, x| acc + x)
}

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

fn create_grid<T: Clone>(size: usize, default: T) -> Grid<T> {
    let mut grid = Vec::with_capacity(size);
    let row: Vec<_> = (0..size).map(|_| default.clone()).collect();
    for _ in 0..grid.capacity() {
        grid.push(row.clone());
    }

    grid
}

fn main() {
    let instructions: Vec<_> = read_input()
        .unwrap()
        .lines()
        .map(Instruction::parse)
        .collect();

    let mut first = create_grid(1000, OneOffLight::Off);
    let mut second = create_grid(1000, 0);

    for instruction in instructions {
        instruction.apply_to(&mut first);
        instruction.apply_to(&mut second);
    }

    println!("Total lights count: {}", lights_count(&first));
    println!("Total brightness: {}", total_brightness(&second));
}
