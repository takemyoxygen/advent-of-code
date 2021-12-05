use std::io;
use std::io::Read;

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

fn has_escaped_backslash(chars: &Vec<char>, index: usize) -> Option<usize> {
    if (index + 1 < chars.len()) &&
       (chars[index] == '\\') &&
       (chars[index + 1] == '\\') {
       Some(index + 2) }
    else {
        None
    }
}

fn has_escaped_quote(chars: &Vec<char>, index: usize) -> Option<usize> {
    if (index + 1 < chars.len()) &&
       (chars[index] == '\\') &&
       (chars[index + 1] == '\"') {
       Some(index + 2) }
    else {
        None
    }
}

fn has_escaped_character(chars: &Vec<char>, index: usize) -> Option<usize> {
    if (index + 3 < chars.len()) &&
       (chars[index] == '\\') &&
       (chars[index + 1] == 'x') &&
       (chars[index + 2].is_digit(16)) &&
       (chars[index + 3].is_digit(16)) {
       Some(index + 4) }
    else {
        None
    }
}

// (code, memory)
fn calculate_decoded_size(line: &str) -> (usize, usize) {
    let mut chars_in_memory = line.len();

    if line.starts_with("\"") { chars_in_memory -= 1; }
    if line.ends_with("\"") { chars_in_memory -= 1; }

    let chars: Vec<_> = line.chars().collect();

    let mut i = 0;
    loop {
        if i == chars.len() { break; }

        if let Some(next) = has_escaped_backslash(&chars, i)
            .or_else(|| has_escaped_quote(&chars, i))
            .or_else(|| has_escaped_character(&chars, i)) {
            chars_in_memory -= next - i - 1;
            i = next;
        } else {
            i = i + 1;
        }
    }

    (line.len(), chars_in_memory)
}

fn calculate_encoded_size(line: &str) -> usize {
    let mut encoded_size = line.len() + 2; // " in the beginning and in the end.
    let chars: Vec<_> = line.chars().collect();

    let mut i = 0;
    loop {
        if i == chars.len() { break; }

        if chars[i] == '\"' {
            encoded_size += 1;
            i += 1;
         } else if let Some(next) = has_escaped_backslash(&chars, i)
            .or_else(|| has_escaped_quote(&chars, i)) {
            encoded_size += 2;
            i = next;
        } else if let Some(next) = has_escaped_character(&chars, i) {
            encoded_size += 1;
            i = next;
        } else {
            i += 1;
        }
    }

    encoded_size
}

fn main() {
    let input = read_input().unwrap();
    let total_and_decoded = input
        .lines()
        .map(calculate_decoded_size)
        .fold(
            (0, 0),
            |(total_and_decoded_code, total_and_decoded_memory), (code, memory)|
                (total_and_decoded_code + code, total_and_decoded_memory + memory));

    let encoded = input
        .lines()
        .map(calculate_encoded_size)
        .fold(0, |acc, x| acc + x);

    println!("Chars in code: {}", total_and_decoded.0);
    println!("Chars in memory when decoded: {}", total_and_decoded.1);
    println!("Diff with decoded: {}", total_and_decoded.0 - total_and_decoded.1);

    println!("Chars when encoded: {}", encoded);
    println!("Diff with encoded: {}", encoded - total_and_decoded.0);
}
