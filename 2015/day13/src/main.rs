use std::io;
use std::io::Read;
use std::str::FromStr;
use std::collections::{HashMap, HashSet};

type Person<'a> = &'a str;
type HappinessTable<'a> = HashMap<(Person<'a>, Person<'a>), i32>;

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

fn parse_input(input: &str) -> Vec<(&str, i32, &str)> {
    input
        .lines()
        .map(|line| {
            let tokens: Vec<_> = line.split_whitespace().collect();
            let name = tokens[0];
            let neigbour = tokens[10].trim_right_matches('.');
            let happiness = i32::from_str(tokens[3]).unwrap();
            let multiplier = if tokens[2] == "gain" { 1 } else { -1 };
            (name, happiness * multiplier, neigbour)
        })
        .collect()
}

fn add<'a>(name1: Person<'a>, name2: Person<'a>, happiness: i32, table: &mut HappinessTable<'a>){
    let mut entry = table.entry((name1, name2)).or_insert(0);
    *entry += happiness;
}

fn build_happiness_table<'a>(rules: &'a Vec<(Person<'a>, i32, Person<'a>)>) -> HappinessTable<'a> {
    let mut table = HappinessTable::new();

    for &(name, happiness, neighbour) in rules {
        add(name, neighbour, happiness, &mut table);
        add(neighbour, name, happiness, &mut table);
    }

    table
}

fn find_path(
    start: Person,
    current: Person,
    happiness_so_far: i32,
    happiness_table: &HappinessTable,
    remaining: &HashSet<Person>) -> i32 {
    if remaining.len() == 1 { // remaining contains only "current"
        happiness_so_far + happiness_table.get(&(start, current)).unwrap()
    } else {
        let mut remaining = remaining.clone();
        remaining.remove(current);
        remaining
            .iter()
            .map(|&next|{
                let happiness_so_far = happiness_so_far + happiness_table.get(&(current, next)).unwrap();
                find_path(start, next, happiness_so_far, happiness_table, &remaining)
            })
            .fold(i32::min_value(), std::cmp::max)
    }
}

fn max_happiness(happiness_table: &HappinessTable) -> i32 {
    let guests: HashSet<_> = happiness_table.keys().map(|&(p, _)| p).collect();

    let start = guests.iter().next().unwrap();

    find_path(&start, &start, 0, happiness_table, &guests)
}

fn add_myself(happiness_table: &mut HappinessTable) {
    let guests: Vec<_> = happiness_table.keys().map(|&(ref p, _)| p).cloned().collect();

    for person in guests {
        add("me", &person, 0, happiness_table);
        add(&person, "me", 0, happiness_table);
    }
}

fn main() {
    let input = read_input().unwrap();
    let happiness_rules = parse_input(&input);

    let mut table = build_happiness_table(&happiness_rules);

    let happiness = max_happiness(&table);
    println!("Max happiness: {}", happiness);

    add_myself(&mut table);
    let happiness = max_happiness(&table);
    println!("Max happiness with myself: {}", happiness);
}
