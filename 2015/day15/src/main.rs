use std::io;
use std::io::Read;
use std::str::FromStr;
use std::cmp;

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

#[derive(Debug)]
struct Ingridient<'a> {
    name: &'a str,
    capacity: i32,
    durability: i32,
    flavour: i32,
    texture: i32,
    calories: i32
}

fn parse_ingridient(line: &str) -> Ingridient {
    let tokens: Vec<_> = line
        .split(|c: char| c == ' ' || c == ',' || c == ':')
        .filter(|s| !s.is_empty())
        .collect();

    Ingridient {
        name: tokens[0],
        capacity: i32::from_str(tokens[2]).unwrap(),
        durability: i32::from_str(tokens[4]).unwrap(),
        flavour: i32::from_str(tokens[6]).unwrap(),
        texture: i32::from_str(tokens[8]).unwrap(),
        calories: i32::from_str(tokens[10]).unwrap()}
}

fn calculate_score(added_ingridients: &Vec<(&Ingridient, usize)>) -> i32 {

    let (capacity, durability, flavour, texture) = added_ingridients
        .iter()
        .fold(
            (0, 0, 0, 0),
            |(cap, dur, flav, text), &(ingr, q)|{
                let q = q as i32;
                (cap + ingr.capacity * q,
                 dur + ingr.durability * q,
                 flav + ingr.flavour * q,
                 text + ingr.texture * q)
            });

    cmp::max(0, capacity) * cmp::max(0, durability) * cmp::max(0, flavour) * cmp::max(0, texture)
}

fn find_best_recipe(
    so_far: Vec<(&Ingridient, usize)>,
    ingridients: & Vec<Ingridient>,
    index: usize,
    remaining_quantity: usize,
    required_calories: Option<i32>) -> Option<i32> {
    if index == ingridients.len() - 1 {
        let mut so_far = so_far.clone();
        so_far.push((&ingridients[index], remaining_quantity));
        match required_calories {
            Some(calories) => {
                let total_calories = so_far.iter().map(|&(i, q)|i.calories*(q as i32)).fold(0, |x, y| x + y);
                if total_calories == calories { Some(calculate_score(&so_far)) }
                else { None }
            },
            _ => Some(calculate_score(&so_far))
        }
    } else {
        (0..remaining_quantity + 1)
            .map(|quantity| {
                let mut so_far = so_far.clone();
                so_far.push((&ingridients[index], quantity));
                find_best_recipe(so_far, ingridients, index + 1, remaining_quantity - quantity, required_calories)
            })
            .max()
            .unwrap()
    }
}

fn main() {
    let input = read_input().unwrap();
    let ingridients: Vec<_> = input.lines().map(parse_ingridient).collect();
    let so_far = Vec::with_capacity(ingridients.len());

    let score = find_best_recipe(so_far.clone(), &ingridients, 0, 100, None);
    let score_with_calories_limit = find_best_recipe(so_far.clone(), &ingridients, 0, 100, Some(500));

    println!("Total score: {:?}", score);
    println!("Total score within 500 calories: {:?}", score_with_calories_limit);
}
