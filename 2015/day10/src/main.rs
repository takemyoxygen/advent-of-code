use std::io;
use std::io::Read;
use std::char;

struct GroupIdenticalElements<I, T>
    where I: Iterator {
    parent: I,
    completed: bool,
    current_group: Vec<T>
}

trait GroupIdentical<T> : Iterator
    where Self: Sized {

    fn group_identical(self) -> GroupIdenticalElements<Self, T> {
        GroupIdenticalElements {parent: self, completed: false, current_group: Vec::new()}
    }
}

impl<'a, I, T> Iterator for GroupIdenticalElements<I, T>
    where I: Iterator<Item=&'a T>,
          T: 'a + Eq + PartialEq + Clone {
    type Item = Vec<T>;

    fn next(&mut self) -> Option<Vec<T>> {
        if self.completed {
            return None;
        }

        let mut result = None;
        loop {
            match self.parent.next() {
                Some(next) => {
                    if self.current_group.is_empty() ||
                       self.current_group[0] == *next {
                        self.current_group.push(next.clone());
                    } else {
                        let x = self.current_group.clone();
                        self.current_group = vec![next.clone()];
                        result = Some(x);
                        break;
                    }
                },
                None if !self.current_group.is_empty() => {
                    self.completed = true;
                    result = Some(self.current_group.clone());
                    break;
                }
                None => { break; }
            }
        }

        result
    }
}

impl<I, T> GroupIdentical<T> for I where I: Iterator { }

fn read_and_say(input: Vec<char>) -> Vec<char> {
    input
        .iter()
        .group_identical()
        .flat_map(|group| {
            let length = char::from_digit(group.len() as u32, 10).unwrap();
            vec![length, group[0]]
        })
        .collect()
}

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

fn main() {
    let input = read_input().unwrap();
    let chars: Vec<_> = input.chars().collect();

    let result_one = (0..40).fold(chars.clone(), |prev, _| read_and_say(prev)).len();
    let result_two = (0..50).fold(chars.clone(), |prev, _| read_and_say(prev)).len();


    println!("Result for 40 repetitions: {:?}", result_one);
    println!("Result for 50 repetitions: {:?}", result_two);
}
