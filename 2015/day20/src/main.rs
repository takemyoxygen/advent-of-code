fn lowest_house_number(target: usize, presents_multiplier: usize, houses_per_elf_limit: Option<usize>) -> Option<usize> {
    let count = target / presents_multiplier;

    let mut houses: Vec<usize> = Vec::with_capacity(count);
    for _ in 0..target{
        houses.push(0);
    }

    let mut result = None;

    for elf in 1..count {
        let mut house = elf;
        let limit = match houses_per_elf_limit {
            Some(l) => elf * l,
            None => count
        };

        while house <= limit && house < houses.len() {
            houses[house] += elf * presents_multiplier;
            house += elf;
        }

        if houses[elf] >= target {
            result = Some(elf);
            break;
        }
    }

    result

}

fn main() {
    let target = 34000000;

    let part1 = lowest_house_number(target, 10, None);
    println!("Part 1. The lowest house number: {:?}", part1);

    let part2 = lowest_house_number(target, 11, Some(50));
    println!("Part 2. The lowest house number: {:?}", part2);
}
