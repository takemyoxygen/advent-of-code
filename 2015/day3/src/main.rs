use std::io;
use std::io::Read;
use std::collections::HashSet;
use std::fmt::{Display, Formatter, Error};

#[derive(Eq, Hash, PartialEq, Clone)]
struct Location {
    x: i32,
    y: i32
}

impl Display for Location {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), Error>{
        write!(formatter, "({}, {})", self.x, self.y)
    }
}

impl Location {

    fn start() -> Location { Location {x: 0, y: 0} }

    fn left(&self) -> Location { Location { x: self.x - 1, y: self.y} }

    fn right(&self) -> Location { Location { x: self.x + 1, y: self.y } }

    fn up(&self) -> Location { Location { x: self.x, y: self.y + 1 } }

    fn down(&self) -> Location { Location { x: self.x, y: self.y -1 } }
}

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

fn next_location(origin: &Location, direction: &char) -> Location {
    match *direction {
        '>' => origin.right(),
        'v' => origin.down(),
        '<' => origin.left(),
        '^' => origin.up(),
        _ => panic!("Unexpected direction: {:?}", direction)
    }
}

fn deliver_present(location: &Location, delivered: &mut HashSet<Location>) {
    delivered.insert(location.clone());
}

fn traverse<T, F>(input: &str, initial: T, traverser: F) -> HashSet<Location>
    where F: Fn(&mut T, char) -> Location {
        let mut delivered = HashSet::new();

        deliver_present(&Location::start(), &mut delivered);

        let visited = input
            .chars()
            .scan(initial, |state, direction| {
                Some(traverser(state, direction))
            });


        for v in visited {
            deliver_present(&v, &mut delivered);
        }

        delivered
    }

fn main() {
    let input = read_input().unwrap();

    let delivered_presents = traverse(&input, Location::start(), |current, direction|{
        *current = next_location(current, &direction);
        current.clone()
    });

    let delivered_with_robo_santa_presents = traverse(
        &input,
        (Location::start(), Location::start()),
        |locations, direction| {
            let (current, next) = locations.clone();
            let moved = next_location(&current, &direction);
            *locations = (next, moved.clone());
            moved.clone()
        });

    println!("Visited houses: {}", delivered_presents.len());
    println!("Visited with robo-santa houses: {}", delivered_with_robo_santa_presents.len());
}
