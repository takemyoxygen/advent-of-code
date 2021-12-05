use std::io::Result;
use std::io::Read;
use std::io;

fn read_input() -> Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

fn char_to_number(c: char) -> i32 {
    match c {
        '(' => 1,
        ')' => -1,
        _ => panic!("Unexpedted symbol {:?}", c)
    }
}

fn calculate_floor(input: &str) -> i32 {
    input
        .chars()
        .map(char_to_number)
        .fold(0, |acc, item| acc + item) // code using "sum()" function doesn't compile
}

fn find_step_to_basement(input: &str) -> usize {
    let mut floor = 0;

    for (index, delta) in input.chars().map(char_to_number).enumerate() {
        floor += delta;
        if floor == -1 {
            return index + 1;
        }
    }

    panic!("Not found");
}

fn main() {
    // let filename = read_file_name().unwrap();
    // let content = read_file(&filename).unwrap();
    let input = read_input().unwrap();
    let floor = calculate_floor(&input);
    let step_to_basement = find_step_to_basement(&input);

    println!("Your input was:");
    println!("{}", input);

    println!("The floor is {}", floor);
    println!("Step to the basement is {}", step_to_basement);
}
