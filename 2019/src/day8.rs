use std::{collections::HashMap, fs};

use crate::solver::Day;

pub struct Day8;

const WIDTH: usize = 25;
const HEIGHT: usize = 6;

const SIZE: usize = WIDTH * HEIGHT;

type Image = Vec<HashMap<(usize, usize), char>>;

fn pos_to_coord(pos: usize) -> (usize, usize, usize) {
    let layer = pos / SIZE;
    let pos_in_layer = pos % SIZE;
    let x = pos_in_layer % WIDTH;
    let y = pos_in_layer / WIDTH;
    return (layer, x, y);
}

fn get_color(image: &Image, layer: usize, pos: (usize, usize)) -> &char {
    let color_in_layer = image[layer].get(&pos).unwrap();
    let layer_is_last = image.len() - 1 == layer;
    match color_in_layer {
        '2' if !layer_is_last => get_color(image, layer + 1, pos),
        _ => color_in_layer,
    }
}

impl Day for Day8 {
    type Intermediate = Image;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        let input = fs::read_to_string(file).unwrap();
        let mut image = Image::with_capacity(input.chars().count() / SIZE + 1);

        for (i, c) in input.chars().enumerate() {
            let (layer, x, y) = pos_to_coord(i);
            if image.len() > layer {
                image[layer].insert((x, y), c);
            } else {
                image.push(HashMap::from([((x, y), c)]));
            }
        }

        image
    }

    fn part1(image: &Self::Intermediate) -> String {
        let target_layer = image
            .iter()
            .map(|layer| {
                let mut counts = HashMap::from([('0', 0), ('1', 0), ('2', 0)]);
                for v in layer.values() {
                    counts.entry(*v).and_modify(|cnt| *cnt += 1);
                }
                counts
            })
            .min_by_key(|counts| *counts.get(&'0').unwrap_or(&0))
            .unwrap();

        (target_layer.get(&'1').unwrap_or(&0) * target_layer.get(&'2').unwrap_or(&0)).to_string()
    }

    fn part2(image: &Self::Intermediate) -> String {
        let mut message = String::with_capacity(SIZE + HEIGHT);

        for y in 0..HEIGHT {
            message.push('\n');
            for x in 0..WIDTH {
                let color = get_color(image, 0, (x, y));
                let printable = if color == &'1' { '*' } else { ' ' };
                message.push(printable);
            }
        }

        message
    }
}
