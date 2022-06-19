use crate::{solver::Day, utils::read_lines};
use std::collections::{HashMap, VecDeque};

pub struct Day6;

fn count_orbits(
    orbits: &HashMap<&String, &String>,
    satellite: &String,
    cache: &mut HashMap<String, u32>,
) -> u32 {
    match (cache.get(satellite), orbits.get(satellite)) {
        (_, None) => 0,
        (Some(val), _) => *val,
        (_, Some(center)) => 1 + count_orbits(orbits, center, cache),
    }
}

fn add_pair<'a>(
    adjacency: &mut HashMap<&'a String, Vec<&'a String>>,
    from: &'a String,
    to: &'a String,
) -> () {
    match adjacency.get_mut(from) {
        None => {
            adjacency.insert(from, vec![to]);
        }
        Some(ex) => {
            ex.push(&to);
        }
    }
}

impl Day for Day6 {
    type Intermediate = Vec<(String, String)>;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        read_lines(file)
            .map(|line| {
                let mut parts = line.split(")");
                (
                    String::from(parts.next().unwrap()),
                    String::from(parts.next().unwrap()),
                )
            })
            .collect()
    }

    fn part1(pairs: &Self::Intermediate) -> String {
        let mut orbits = HashMap::new();
        for (center, satellite) in pairs {
            orbits.insert(satellite, center);
        }

        let mut cache = HashMap::new();
        orbits
            .keys()
            .map(|key| count_orbits(&orbits, key, &mut cache))
            .sum::<u32>()
            .to_string()
    }

    fn part2(pairs: &Self::Intermediate) -> String {
        let mut adjacency = HashMap::new();

        for (center, satellite) in pairs {
            add_pair(&mut adjacency, center, satellite);
            add_pair(&mut adjacency, satellite, center);
        }

        let start = *adjacency
            .get(&String::from("YOU"))
            .unwrap()
            .first()
            .unwrap();

        let finish = *adjacency
            .get(&String::from("SAN"))
            .unwrap()
            .first()
            .unwrap();

        let mut queue = VecDeque::from([start]);
        let mut steps_to_nodes = HashMap::from([(start, 0)]);

        while !queue.is_empty() {
            let node = queue.pop_front().unwrap();
            let steps = *steps_to_nodes.get(node).unwrap();

            if node == finish {
                return steps.to_string();
            }

            for adj in adjacency.get(node).unwrap().iter() {
                if steps_to_nodes.get(*adj).is_none() {
                    steps_to_nodes.insert(*adj, steps + 1);
                    queue.push_back(*adj);
                }
            }
        }

        panic!("Can't find path!!!")
    }
}
