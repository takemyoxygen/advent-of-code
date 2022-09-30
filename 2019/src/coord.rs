pub trait VecXD {
    fn add(&self, another: &Self) -> Self;
    fn zero() -> Self;
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Vec2D {
    pub x: i32,
    pub y: i32,
}

impl VecXD for Vec2D {
    fn add(&self, another: &Self) -> Self {
        Vec2D {
            x: self.x + another.x,
            y: self.y + another.y,
        }
    }

    fn zero() -> Self {
        Vec2D { x: 0, y: 0 }
    }
}

// in clockwise order
const DIRECTIONS: [&'static Vec2D; 4] = [
    &Vec2D { x: 0, y: 1 },
    &Vec2D { x: 1, y: 0 },
    &Vec2D { x: 0, y: -1 },
    &Vec2D { x: -1, y: 0 },
];

fn dir_index(direction: &Vec2D) -> usize {
    match DIRECTIONS.iter().position(|&d| d == direction) {
        Some(index) => index,
        _ => panic!("Invalid direction value provided: {:?}", direction),
    }
}

pub fn right(direction: &Vec2D) -> &Vec2D {
    let index = dir_index(direction);
    DIRECTIONS[(index + 1) % DIRECTIONS.len()]
}

pub fn left(direction: &Vec2D) -> &Vec2D {
    let index = dir_index(direction);
    DIRECTIONS[(DIRECTIONS.len() + index - 1) % DIRECTIONS.len()]
}
