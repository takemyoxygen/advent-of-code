use std::io;
use std::io::Read;
use std::collections::HashMap;
use std::str::FromStr;

type DynamicInfo<'a> = HashMap<&'a str, u8>;

#[derive(Debug)]
struct Aunt<'a> {
    number: u16,
    info: DynamicInfo<'a>
}

fn parse_aunt(line: &str) -> Aunt {
    let tokens: Vec<_> = line
        .split(|c: char| !c.is_alphanumeric())
        .filter(|c| !c.is_empty())
        .collect();

    let mut aunt = Aunt { number: u16::from_str(tokens[1]).unwrap(), info: HashMap::new() };

    for i in 0..((tokens.len() - 2) / 2) {
        aunt.info.insert(tokens[2 * i + 2], u8::from_str(tokens[2 * i + 3]).unwrap());
    }

    aunt
}

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

fn matches(aunt: &Aunt, specification: &DynamicInfo) -> bool {
    let ref info = aunt.info;
    info
        .into_iter()
        .all(|(attribute, value)| {
            match specification.get(attribute) {
                Some(x) if x == value => true,
                _ => false
            }
        })
}

fn matches_adjusted(aunt: &Aunt, specification: &DynamicInfo) -> bool {
    let ref info = aunt.info;
    info
        .into_iter()
        .all(|(attribute, value)| {
            match (*attribute, specification.get(attribute)) {
                ("cats", Some(x)) | ("trees", Some(x)) => x < value,
                ("pomeranians", Some(x)) | ("goldfish", Some(x)) => x > value,
                (_, Some(x)) => x == value,
                _ => false
            }
        })
}


fn main() {
    let input = read_input().unwrap();
    let aunts: Vec<_> = input.lines().map(parse_aunt).collect();

    let mut machine_output = HashMap::with_capacity(10);
    machine_output.insert("children", 3);
    machine_output.insert("cats", 7);
    machine_output.insert("samoyeds", 2);
    machine_output.insert("pomeranians", 3);
    machine_output.insert("akitas", 0);
    machine_output.insert("vizslas", 0);
    machine_output.insert("goldfish", 5);
    machine_output.insert("trees", 2);
    machine_output.insert("cars", 3);
    machine_output.insert("perfumes", 1);

    let aunt_sue = aunts.iter().find(|aunt| matches(aunt, &machine_output));
    println!("Aunt Sue: {:?}", aunt_sue);

    let another_aunt_sue = aunts.iter().find(|aunt| matches_adjusted(aunt, &machine_output));
    println!("Another Aunt Sue: {:?}", another_aunt_sue);

}
