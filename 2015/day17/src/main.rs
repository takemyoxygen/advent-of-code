use std::io;
use std::io::Read;
use std::str::FromStr;

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

// (# of containers, # of different ways discovered so far to fill this number of containers)
type Filling = (usize, usize);

fn combinations_count(containers: &Vec<usize>, index: usize, volume: usize, containers_filled: usize, best_filling: Filling) -> (usize, Filling) {
    if index == containers.len() {
        if volume == 0 {
             if best_filling.0 == containers_filled { (1, (best_filling.0, best_filling.1 + 1)) }
             else if best_filling.0 > containers_filled { (1, (containers_filled, 1)) }
             else { (1, best_filling) }
         } else { (0, best_filling) }
    }
    else if containers[index] > volume {
        combinations_count(containers, index + 1, volume, containers_filled, best_filling)
    }
    else {
        let (combinations_when_current_is_filled, filling) = combinations_count(
            &containers,
            index + 1,
            volume - containers[index],
            containers_filled + 1,
            best_filling);

        let (combination_when_current_is_empty, filling) = combinations_count(
            &containers,
            index + 1,
            volume,
            containers_filled,
            filling);

        (combination_when_current_is_empty + combinations_when_current_is_filled, filling)
    }
}

fn main() {
    let input = read_input().unwrap();
    let mut containers: Vec<_> = input.lines().map(|x| usize::from_str(x).unwrap()).collect();
    containers.sort_by(|a, b| a.cmp(b).reverse());

    let (total_combinations, (min_containers_filled, combinations)) = combinations_count(
        &containers,
        0,
        150,
        0,
        (usize::max_value(), 0));

    println!("Containers: {:?}", containers);

    println!(
        "Total Combinations count: {}. Min # of containers: {} ({} combinations)",
        total_combinations,
        min_containers_filled,
        combinations);
}
