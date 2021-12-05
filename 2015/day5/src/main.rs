use std::io;
use std::io::Result;
use std::io::Read;

fn read_input() -> Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

fn is_vowel(c: &char) -> bool {
    match *c {
        'a' | 'e' | 'i' | 'o' | 'u' => true,
        _ => false
    }
}

fn is_forbidden(x: &char, y: &char) -> bool {
    match (*x, *y) {
        ('a', 'b') | ('c', 'd') | ('p', 'q') | ('x', 'y') => true,
        _ => false
    }
}

fn is_nice_stats(stats: &NicenessStats) -> bool {
    stats.vowels >= 3 && stats.has_duplicate_letters && !stats.has_forbidden_strings
}

fn pairwise(string: &str) -> Vec<(Option<char>, char)> {
    string.chars().scan(None, |prev, current| {
        let before = prev.clone();
        *prev = Some(current);
        Some((before, current))
    })
    .collect()
}

#[derive(Debug)]
struct NicenessStats {
    vowels: u8,
    has_duplicate_letters: bool,
    has_forbidden_strings: bool
}

fn is_nice(string: &str) -> bool {
    let stats = pairwise(string)
        .iter()
        .fold(
            NicenessStats {vowels: 0, has_duplicate_letters: false, has_forbidden_strings: false},
            |stats, &(prev, current)| {
            let vowels = stats.vowels + (if is_vowel(&current) { 1 } else { 0 });
            let not_first = prev.is_some();
            let has_duplicate_letters =
                stats.has_duplicate_letters ||
                (not_first && prev.unwrap() == current);

            let has_forbidden_strings =
                stats.has_forbidden_strings ||
                (not_first && is_forbidden(&prev.unwrap(), &current));

            NicenessStats
                { vowels: vowels,
                  has_duplicate_letters: has_duplicate_letters,
                  has_forbidden_strings: has_forbidden_strings }
        });

    is_nice_stats(&stats)
}

fn is_even_nicer(string: &str) -> bool {
    let chars: Vec<_> = string.chars().collect();

    let mut has_repeated_pair = false;
    let mut has_same_letter_separated_by_another = false;

    for i in 0..chars.len() - 2 {

        for j in i+2..chars.len() - 1 {
            if chars[i] == chars[j] && chars[i + 1] == chars[j + 1] {
                has_repeated_pair = true;
                break;
            }
        }

        if chars[i] == chars[i + 2] {
            has_same_letter_separated_by_another = true;
        }
    }

    has_repeated_pair && has_same_letter_separated_by_another
}

fn main() {
    let input = read_input().unwrap();

    let nice = input
        .lines()
        .filter(|s| is_nice(s))
        .count();

    let nicer = input
        .lines()
        .filter(|s| is_even_nicer(s))
        .count();

    println!("Nice strings count: {}. Even nicer : {}", nice, nicer);
}
