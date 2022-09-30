use crate::{solver::Day, utils};
use regex::Regex;

pub struct Day12;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MoonState<const SIZE: usize> {
    position: [i32; SIZE],
    velocity: [i32; SIZE],
}

fn apply_gravity<const SIZE: usize>(moon: &mut MoonState<SIZE>, another: &MoonState<SIZE>) {
    for i in 0..moon.position.len() {
        if moon.position[i] > another.position[i] {
            moon.velocity[i] -= 1;
        } else if moon.position[i] < another.position[i] {
            moon.velocity[i] += 1;
        }
    }
}

fn apply_velocity<const SIZE: usize>(moon: &mut MoonState<SIZE>) {
    for i in 0..moon.position.len() {
        moon.position[i] += moon.velocity[i];
    }
}

fn abs_sum<const SIZE: usize>(xs: &[i32; SIZE]) -> i32 {
    xs.iter().map(|&x| i32::abs(x)).sum()
}

fn step<const SIZE: usize>(moons: &mut Vec<MoonState<SIZE>>) {
    for i in 0..moons.len() {
        for j in (0..moons.len()).filter(|&j| j != i) {
            let another = moons[j];
            let moon = moons.get_mut(i).unwrap();
            apply_gravity(moon, &another);
        }
    }

    for moon in moons.iter_mut() {
        apply_velocity(moon);
    }
}

fn find_loop<const SIZE: usize>(moons: Vec<MoonState<SIZE>>) -> usize {
    let init_moons = moons.clone();
    let mut current_moons = moons;
    for steps in 1.. {
        step(&mut current_moons);

        if current_moons
            .iter()
            .zip(init_moons.iter())
            .all(|(m1, m2)| m1 == m2)
        {
            return steps;
        }
    }

    panic!("You shall not reach here")
}

impl Day for Day12 {
    type Intermediate = Vec<MoonState<3>>;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        let regex = Regex::new(r"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>").unwrap();
        utils::read_lines(file)
            .map(|line| {
                let captures = regex.captures(&line).unwrap();
                [
                    str::parse(captures.get(1).unwrap().as_str()).unwrap(),
                    str::parse(captures.get(2).unwrap().as_str()).unwrap(),
                    str::parse(captures.get(3).unwrap().as_str()).unwrap(),
                ]
            })
            .map(|pos| MoonState {
                position: pos.clone(),
                velocity: [0, 0, 0],
            })
            .collect::<Vec<MoonState<3>>>()
    }

    fn part1(input: &Self::Intermediate) -> String {
        let mut moons = input.clone();

        for _ in 0..1000 {
            step(&mut moons);
        }

        let energy = moons
            .iter()
            .map(|moon| abs_sum(&moon.position) * abs_sum(&moon.velocity))
            .sum::<i32>();

        energy.to_string()
    }

    fn part2(input: &Self::Intermediate) -> String {
        let steps = (0..input[0].position.len()).map(|i| {
            let single_coord_moons = input
                .iter()
                .map(|moon| MoonState {
                    position: [moon.position[i]],
                    velocity: [moon.velocity[i]],
                })
                .collect::<Vec<MoonState<1>>>();
            find_loop(single_coord_moons)
        });

        utils::least_common_multiple_n(steps).unwrap().to_string()
    }
}
