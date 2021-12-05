extern crate md5;

use std::io;
use std::io::Read;
use md5::Digest;

struct StopWhenIterator<I, P> {
    parent: I,
    predicate: P,
    completed: bool
}

trait StopWhen<P: FnMut(&Self::Item) -> bool> : Iterator
    where Self:Sized {

    /// Iterates over items untill while the given predicate is evaluated to true
    /// Once predicate is evaluated to false, iterator yields current item and completes.
    fn stop_when(self, predicate: P) -> StopWhenIterator<Self, P> {
        StopWhenIterator {parent: self, predicate: predicate, completed: false}
    }
}

impl<I, P> StopWhen<P> for I
    where P: FnMut(&I::Item) -> bool,
          I : Iterator { }

impl<I, P> Iterator for StopWhenIterator<I, P>
    where P: FnMut(&I::Item) -> bool,
          I: Iterator {

    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        if self.completed {
             None
        } else {
            self.parent.next().and_then(|x| {
                if (self.predicate)(&x) {
                    self.completed = true;
                }
                Some(x)
            })
        }
    }
}

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

fn starts_with_five_zeros(digest: &Digest) -> bool {
    digest[0] == 0 && digest[1] == 0 && digest[2] < 16
}

fn sixth_item_is_also_zero(digest: &Digest) -> bool {
    digest[2] == 0
}

fn main() {
    let input = read_input().unwrap();

    let start_with_zero: Vec<_> = (0..i64::max_value())
        .map(|i| (i, input.clone() + &i.to_string()))
        .map(|(i, s)| (i, md5::compute(s.as_bytes())))
        .filter(|&(_, digest)| starts_with_five_zeros(&digest))
        .stop_when(|&(_, digest)| sixth_item_is_also_zero(&digest))
        .map(|(i, _)| i)
        .collect();

    println!("Smallest with 5 zeroes: {}", start_with_zero[0]);
    println!("Smallest with 6 zeroes: {}", start_with_zero[start_with_zero.len() - 1]);
}
