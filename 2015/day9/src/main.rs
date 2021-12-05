use std::collections::{HashMap, HashSet};
use std::io;
use std::io::Read;
use std::str::FromStr;
use std::rc::Rc;

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

type City = String;
type Distances = HashMap<(City, City), usize>;

fn visit<F>(
    city: &City,
    so_far: usize,
    remaining: &HashSet<City>,
    distances: &Distances,
    compare: Rc<F>,
    extreme: usize) -> usize
    where F: Fn(usize, usize) -> usize {
    if remaining.len() == 1 {
        so_far
    } else {
        let mut remaining = remaining.clone();
        remaining.remove(city);
        remaining
            .iter()
            .map(|next| visit(
                &next,
                so_far + distances.get(&(city.clone(), next.clone())).unwrap(),
                &remaining,
                distances,
                compare.clone(),
                extreme))
            .fold(extreme, |acc, x| compare(acc, x))
    }
}

fn parse_distances(input: &str) -> Distances {
    let mut distances = HashMap::new();

    for line in input.lines(){
        let tokens: Vec<_> = line.split_whitespace().collect();
        let origin = tokens[0].to_string();
        let destination = tokens[2].to_string();
        let distance = usize::from_str(tokens[4]).unwrap();
        distances.insert((origin.clone(), destination.clone()), distance);
        distances.insert((destination.clone(), origin.clone()), distance);
    }

    distances
}

fn find_path<F>(
    cities: &HashSet<City>,
    distances: &Distances,
    compare: Rc<F>,
    extreme: usize)
    -> usize
    where F: Fn(usize, usize) -> usize {
        cities
            .iter()
            .map(|city| visit(city, 0, &cities, &distances, compare.clone(), extreme))
            .fold(extreme, |x, y| compare(x, y))
    }

fn main() {
    let input = read_input().unwrap();
    let distances = parse_distances(&input);
    let mut cities = HashSet::new();

    for &(ref origin, ref destination) in distances.keys() {
        cities.insert(origin.clone());
        cities.insert(destination.clone());
    }

    let shortest = find_path(&cities, &distances, Rc::new(std::cmp::min), std::usize::MAX);
    let longest = find_path(&cities, &distances, Rc::new(std::cmp::max), std::usize::MIN);

    println!("Shortest: {}, longest: {}", shortest, longest);
}
