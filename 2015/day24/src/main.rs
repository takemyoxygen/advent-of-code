use std::str::FromStr;

fn group_by_sum(packages: &[usize], target: usize) -> Vec<Vec<usize>> {
    let mut results = Vec::new();
    struct Iter<'a> { f: &'a Fn(&mut Iter, usize, usize, Vec<usize>) -> (), results: &'a mut Vec<Vec<usize>> }

    {
        let mut iter = Iter {results: &mut results,  f: &|iter, index, so_far, current|{
            if so_far == target {
                iter.results.push(current.clone());
            } else if index < packages.len() {
                if packages[index] <= target - so_far {
                    let mut current = current.clone();
                    current.push(index);
                    (iter.f)(iter, index + 1, so_far + packages[index], current);
                }

                (iter.f)(iter, index + 1, so_far, current);
            }
        }};

        (iter.f)(&mut iter, 0, 0, Vec::new());
    }

    results
}

fn intersects(a: &Vec<usize>, b: &Vec<usize>) -> bool {
    a.iter().any(|x| b.contains(x))
}

fn quantum_entanglement(xs: &Vec<usize>) -> usize {
    xs.iter().fold(1, |acc, x| acc * x)
}

fn split_into_groups(packages: &Vec<usize>, target_weight: usize) -> Vec<Vec<usize>> {
    let mut grouped_by_sum: Vec<Vec<usize>> = group_by_sum(&packages, target_weight)
        .iter()
        // assuming that values are unique.
        .map(|combination| combination.iter().map(|i| packages[*i]).collect())
        .collect();

    grouped_by_sum.sort_by(|a, b| (a.len(), quantum_entanglement(a)).cmp(&(b.len(), quantum_entanglement(b))));

    grouped_by_sum
}

fn main() {
    let input = include_str!("../input.txt");
    let packages: Vec<_> = input.lines().map(|i|usize::from_str(i).unwrap()).rev().collect();
    let sum = packages.iter().fold(0, |acc, x| acc + x);

    let target = sum / 3;
    let grouped_by_sum = split_into_groups(&packages, target);

    for group in grouped_by_sum.iter() {
        if grouped_by_sum.iter().any(|another| intersects(group, another)){
            println!("Part 1. Min quantum entanglement: {}", quantum_entanglement(&group));
            break;
        }
    }

    let target = sum / 4;
    let grouped_by_sum = split_into_groups(&packages, target);

    'outer: for first in grouped_by_sum.iter() {
        for second in grouped_by_sum.iter() {
            for third in grouped_by_sum.iter() {
                if !intersects(first, second) &&
                   !intersects(second, third) &&
                   !intersects(third, first) {
                    println!("Part 2. Min quantum entanglement: {}", quantum_entanglement(&first));
                    break 'outer;
                   }
            }
        }
    }
}
