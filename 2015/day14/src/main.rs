use std::io;
use std::io::Read;
use std::str::FromStr;
use std::fmt;
use std::fmt::Display;

#[derive(Debug)]
enum ReindeerState{
    Flying(usize),
    Rest(usize)
}

#[derive(Debug)]
struct Reindeer<'a> {
    name: &'a str,
    speed: usize,
    fly_time: usize,
    rest_time: usize,
    state: ReindeerState,
    distance: usize,
    score: usize
}

impl<'a> Reindeer<'a> {

    fn new(name: &str, speed: usize, fly_time: usize, rest_time: usize) -> Reindeer {
        Reindeer {
            name: name,
            speed: speed,
            fly_time: fly_time,
            rest_time: rest_time,
            state: ReindeerState::Flying(0),
            distance: 0,
            score: 0 }
    }

    fn advance(&mut self) {
        match self.state {
            ReindeerState::Flying(time) => {
                let time = time + 1;
                (*self).distance += self.speed;
                self.state = if time == self.fly_time { ReindeerState::Rest(0) } else { ReindeerState::Flying(time) };
            }
            ReindeerState::Rest(time) => {
                let time = time + 1;
                self.state = if time == self.rest_time { ReindeerState::Flying(0) } else { ReindeerState::Rest(time) };
            }
        }
    }

    fn with_longest_distance<'b, 'c>(deers: &'b Vec<Reindeer<'c>>) -> &'b Reindeer<'c> {
        deers
            .iter()
            .max_by_key(|deer| deer.distance)
            .unwrap()
    }

    fn with_biggest_score<'b, 'c>(deers: &'b Vec<Reindeer<'c>>) -> &'b Reindeer<'c> {
        deers
            .iter()
            .max_by_key(|deer| deer.score)
            .unwrap()
    }

    fn advance_all(deers: &mut Vec<Reindeer>) {
        for deer in deers.iter_mut() {
            deer.advance();
        }

        let distance = { Reindeer::with_longest_distance(deers).distance };

        for deer in deers.iter_mut().filter(|deer| deer.distance == distance) {
            deer.score += 1;
        }
    }
}

impl<'a> Display for Reindeer<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: distance: {} km, score: {}", self.name, self.distance, self.score)
    }
}

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

fn parse_line(line: &str) -> Reindeer {
    let tokens: Vec<_> = line.split_whitespace().collect();
    Reindeer::new(
        tokens[0],
        usize::from_str(tokens[3]).unwrap(),
        usize::from_str(tokens[6]).unwrap(),
        usize::from_str(tokens[13]).unwrap())
}

fn main() {
    let input = read_input().unwrap();
    let mut deers: Vec<_> = input.lines().map(parse_line).collect();
    let time = 2503 ;

    for _ in 0..time {
        Reindeer::advance_all(&mut deers)
    }

    println!("All reindeers:", );
    for deer in &deers {
        println!("{}", deer);
    }

    {
        let winner = Reindeer::with_longest_distance(&mut deers);
        println!("Reindeer with longest distance is {}", winner);
    }

    {
        let winner = Reindeer::with_biggest_score(&mut deers);
        println!("Reindeer with biggest score is {}", winner);
    }
}
