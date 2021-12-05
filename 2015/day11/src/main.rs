use std::io;
use std::io::Read;

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

fn has_increasing_subsequence(chars: &Vec<char>, index: usize) -> bool {
    index + 2 < chars.len() &&
    (chars[index + 1] as i16) - (chars[index] as i16) == 1 &&
    (chars[index + 2] as i16) - (chars[index + 1] as i16) == 1
}

fn has_forbidden_character(chars: &Vec<char>, index: usize) -> bool {
    chars[index] == 'i' ||
    chars[index] == 'o' ||
    chars[index] == 'l'
}

fn has_pair(chars: &Vec<char>, index: usize) -> Option<char> {
    if index + 1 < chars.len() && chars[index] == chars[index + 1] {
        Some(chars[index])
    } else {
        None
    }
}

fn matches_additional_requirements(password: &String) -> bool {
    let chars: Vec<_> = password.chars().collect();

    let mut has_inc_subseq = false;

    let mut first_pair_index = None;
    let mut pairs_count = 0;

    for index in 0..chars.len() {
        if !has_inc_subseq { has_inc_subseq = has_increasing_subsequence(&chars, index); }
        if has_forbidden_character(&chars, index) { return false; }

        if (pairs_count < 2) && has_pair(&chars, index).is_some() {
            if first_pair_index.is_some() && index - first_pair_index.unwrap() >=2 {
                pairs_count += 1;
            } else if first_pair_index.is_none() {
                first_pair_index = Some(index);
                pairs_count += 1;
            }
        }
    }

    has_inc_subseq && (pairs_count >= 2)
}

fn inc(password: &String) -> String {
    let mut bytes = password.clone().into_bytes();
    let z = 'z' as u8;
    let a = 'a' as u8;
    let mut to_add = 1;

    for i in 0..bytes.len() {
        let index = bytes.len() - 1 - i;
        if to_add == 0 {
            break;
        } else {
            bytes[index] += to_add;
            to_add = 0;

            if bytes[index] > z {
                bytes[index] = a;
                to_add = 1;
            }
        }
    }

    String::from_utf8(bytes).unwrap()
}

fn next_after(password: &str) -> String {

    let mut current = password.to_string();

    for _ in 1.. {
        current = inc(&current);
        if matches_additional_requirements(&current) {
            break;
        }
    }

    current
}

fn main() {
    let input = read_input().unwrap();
    
    let next_password = next_after(&input);
    println!("Next password: {}", next_password);

    let very_next_password = next_after(&next_password);
    println!("And when it expires, use that one: {}", very_next_password);
}
