use std::collections::HashSet;

type Light = (i32, i32);
type TurnedOnLights = HashSet<Light>;

fn parse_input(input: &str) -> TurnedOnLights {
    let mut turned_on = TurnedOnLights::new();

    for (i, line) in input.lines().enumerate(){
        for (j, c) in line.chars().enumerate() {
            if c == '#' {
                turned_on.insert((i as i32, j as i32));
            }
        }
    }

    turned_on
}

fn turned_on_neighbours(light: &Light, turned_on: &TurnedOnLights) -> usize {
    let (x, y) = *light;
    let xs = [x - 1, x, x + 1];
    let ys = [y - 1, y, y + 1];

    xs.iter()
        .flat_map(|x1| ys.iter().map(move |y1| (*x1, *y1)))
        .filter(|&(x1, y1)| !(x1 == x && y1 == y))
        .filter(|light| turned_on.contains(light))
        .count()
}

fn next(turned_on: &TurnedOnLights, all: &Vec<Light>) -> TurnedOnLights {
     all.iter()
        .filter(|&light|  {
            let neighbours = turned_on_neighbours(light, turned_on);
            if turned_on.contains(light) && (neighbours == 2 || neighbours == 3) { true }
            else if !turned_on.contains(light) && neighbours == 3 { true }
            else { false }
        })
        .cloned()
        .collect()
}

fn main() {
    let input = include_str!("../input.txt");
    let size = input.lines().count() as i32;
    let turned_on = parse_input(&input);
    let all: Vec<_> = (0..size).flat_map(|i| (0..size).map(move |j| (i, j))).collect();
    let steps = 100;

    let turn_on_corners = |turned_on: &mut TurnedOnLights| {
        turned_on.insert((0, 0));
        turned_on.insert((0, size - 1));
        turned_on.insert((size - 1, 0));
        turned_on.insert((size - 1, size - 1));
    };

    let result = (0..steps).fold(turned_on.clone(), |turned_on, _| next(&turned_on, &all));
    println!("Turned on lights after {} iterations: {}", steps, result.len());

    let result_with_turned_on_corner = (0..steps).fold(turned_on.clone(), |turned_on, _| {
        let mut turned_on_so_far = next(&turned_on, &all);
        turn_on_corners(&mut turned_on_so_far);
        turned_on_so_far
    });

    println!("Turned on lights after {} iterations when corner lights are always on: {}", steps, result_with_turned_on_corner.len());
}
