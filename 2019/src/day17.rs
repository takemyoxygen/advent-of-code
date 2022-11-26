use std::collections::HashSet;

use crate::{intcode, solver::Day};

pub struct Day17;

type Map = Vec<Vec<String>>;

fn output_to_map<'a>(output: &Vec<i64>) -> Map {
    let allowlist: HashSet<&str> = HashSet::from_iter(vec!["#", ".", "<", ">", "^", "v"]);
    let chars = output.clone().iter().map(|x| *x as u8).collect::<Vec<_>>();

    std::str::from_utf8(chars.as_slice())
        .unwrap()
        .split("\n")
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.split("")
                .filter(|c| allowlist.contains(c))
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
}

fn run_robot(program: &intcode::Program, input: &Vec<String>) -> Vec<i64> {
    let input = input
        .iter()
        .flat_map(|line| {
            let mut ll = line.clone();
            ll.push('\n');
            ll.as_bytes().to_vec()
        })
        .map(|b| b as i64)
        .collect::<Vec<_>>();
    let result = intcode::run_program(program, input);
    result.get_output().clone()
}

impl Day for Day17 {
    type Intermediate = intcode::Program;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        intcode::read_program(file)
    }

    fn part1(program: &Self::Intermediate) -> String {
        let result = intcode::run_program(program, vec![]);
        let map = output_to_map(result.get_output());

        let mut result = 0;
        for i in 0..map.len() {
            for j in 0..map[i].len() {
                if i > 0
                    && j > 0
                    && i < map.len() - 1
                    && j < map[i].len() - 1
                    && map[i][j] == "#"
                    && map[i - 1][j] == "#"
                    && map[i + 1][j] == "#"
                    && map[i][j - 1] == "#"
                    && map[i][j + 1] == "#"
                {
                    result += j * i;
                }
            }
        }

        result.to_string()
    }

    fn part2(original_program: &Self::Intermediate) -> String {
        let mut program = original_program.clone();
        program[0] = 2;

        let output = run_robot(
            &program,
            &vec![
                String::from("A,B,A,C,B,A,C,B,A,C"),
                String::from("L,6,L,4,R,12,L"),
                String::from("6,R,12,R,12,L,8"),
                String::from("6,L,10,L,10,L,6,L"),
                String::from("n"),
            ],
        );

        output.last().unwrap().to_string()
    }
}
