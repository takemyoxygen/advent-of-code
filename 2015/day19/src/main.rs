use std::collections::HashSet;
use std::str::from_utf8;

fn parse_replacements(input: &str) -> Vec<(String, String)> {
    input.lines()
        .map(|line|line.split(" => ").collect::<Vec<_>>())
        .filter(|tokens| tokens.len() == 2)
        .map(|tokens|(tokens[0].to_string(), tokens[1].to_string()))
        .collect()
}

fn replace(molecule: &str, replacement: &(String, String), results: &mut HashSet<String>) {
    let &(ref from, ref to) = replacement;
    let start_positions = molecule
        .as_bytes()
        .windows(from.len())
        .enumerate()
        .filter(|&(_, window)|  from == from_utf8(window).unwrap())
        .map(|(index, _)| index);

    for start in start_positions {
        let mut replaced = molecule[0..start].to_owned();
        replaced.extend(to.chars());
        replaced.extend(molecule[start + from.len()..].chars());
        results.insert(replaced);
    }
}

fn apply_replacements(molecule: &str, replacements: &Vec<(String, String)>) -> HashSet<String> {
    let mut results = HashSet::new();

    for replacement in replacements {
        replace(molecule, replacement, &mut results);
    }

    results
}

fn reverse_and_sort(replacements: &Vec<(String, String)>) -> Vec<(String, String)> {
    let mut reversed: Vec<_> = replacements.iter().map(|&(ref from, ref to)| (to.clone(), from.clone())).collect();
    reversed.sort_by(|a, b| a.0.len().cmp(&b.0.len()).reverse());
    reversed
}

fn apply_replacements_consequently(input: &str, replacements: &Vec<(String, String)>) -> (String, usize) {
    let mut previous = input.to_owned();
    let mut replacements_count = 0;

    loop {
        let mut current = previous.clone();

        for &(ref from, ref to) in replacements {
            loop {
                match current.find(from){
                    Some(index) => {
                        let before = current[..index].to_string();
                        current = before + to + &current[index + from.len()..];
                        replacements_count += 1;
                    }
                    None => { break; }
                }
            }
        }

        if previous == current { break; }
        previous = current;
    }

    (previous, replacements_count)
}

fn main() {
    let input = include_str!("../input.txt");
    let replacements = parse_replacements(&input);
    let molecule = input.lines().last().unwrap();

    let derived_molecules = apply_replacements(molecule, &replacements);
    println!("Part 1: different molecules found: {}", derived_molecules.len());

    let replacements_reversed = reverse_and_sort(&replacements);

    let mut current = molecule.to_string();
    let mut total_replacements = 0;

    for iteration in 0.. {
        println!(
            "Iteration #{}, replacements so far: {}, chars: {}, content: {}\r\n",
            iteration,
            total_replacements,
            current.chars().count(),
            current);

        let (minimized, replacements) = apply_replacements_consequently(&current, &replacements_reversed);
        current = minimized;
        total_replacements += replacements;

        if current == "e" { break; }
    }

    println!("Part 2: replacements required to create a molecule from \"e\": {}", total_replacements);
}
