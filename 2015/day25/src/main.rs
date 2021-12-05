fn next(current: usize) -> usize {
    (current * 252533) % 33554393
}

fn calculate(row: usize, column: usize) -> usize {
    let diagonal = row + column - 1;
    let full_diagonals = diagonal - 1;
    let number = full_diagonals * (full_diagonals + 1) / 2 + column;

    let mut current = 20151125;
    for _ in 0..number - 1 {
        current = next(current);
    }

    current
}

fn main() {
    let row = 2978;
    let column = 3083;

    println!("Part 1: {}", calculate(row, column));
}
