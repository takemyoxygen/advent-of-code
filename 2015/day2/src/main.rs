use std::io;
use std::io::Read;
use std::num::ParseIntError;
use std::str::FromStr;

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

fn sum<'a, T: Iterator<Item=&'a u32>>(iter: T) -> u32 {
    iter.fold(0, |acc, x| acc + x)
}

fn parse_line(line: &str) -> Result<(u32, u32, u32), ParseIntError> {
    let tokens: Vec<_> = line.trim().split('x').map(u32::from_str).collect();

    if tokens.len() != 3 {
        panic!("Unexpected dimensions count");
    }

    tokens[0].clone().and_then(
        |x| tokens[1].clone().and_then(
            |y| tokens[2].clone().map(|z| (x, y, z))))
}

fn calculate_area(l: u32, w: u32, h: u32) -> u32 {
    let areas = [l * w, h * w, l * h];
    let extra = areas.iter().min().unwrap();
    let area = 2 * sum(areas.iter());
    area + extra
}

fn calculate_ribbon_length(l: u32, w: u32, h: u32) -> u32 {
    let wrap = 2 * [l + w, w + h, h + l].iter().min().unwrap();
    let bow = l * w * h;
    wrap + bow
}

fn main() {
    let input = read_input().unwrap();
    let gifts: Vec<_> = input
        .lines()
        .map(parse_line)
        .map(|dim| dim.unwrap())
        .collect();

    let areas: Vec<_> = gifts
        .iter()
        .map(|&(l, w, h)| calculate_area(l, w, h))
        .collect();

    let ribbons_length: Vec<_> = gifts
        .iter()
        .map(|&(l, w, h)| calculate_ribbon_length(l, w, h))
        .collect();

    let total_area = sum(areas.iter());
    let total_ribbon_length = sum(ribbons_length.iter());

    println!("Total area: {}", total_area);
    println!("Total ribbon length: {}", total_ribbon_length);
}
