use std::{fs, io::stdin};

use im::HashSet;
use itertools::Itertools;

use crate::{
    intcode::{self, WaitingForInputProgram},
    solver::Day,
};

pub struct Day25;

const ITEMS: [&'static str; 7] = [
    "hypercube",
    "space law space brochure",
    "festive hat",
    "shell",
    "astronaut ice cream",
    "mug",
    "whirled peas",
];

const SAVE_FILENAME: &'static str = "./day25.save.txt";

fn output_as_string(output: &Vec<i64>) -> String {
    String::from_utf8(output.iter().map(|c| *c as u8).collect()).unwrap()
}

fn command_as_input(command: String) -> Vec<i64> {
    command
        .as_bytes()
        .to_vec()
        .into_iter()
        .map(|b| b as i64)
        .collect_vec()
}

fn read_command() -> String {
    let mut buffer = String::new();
    println!("Your command: ");
    stdin().read_line(&mut buffer).unwrap();
    buffer
}

fn send_command(state: &WaitingForInputProgram, command: &str) -> (intcode::ProgramResult, String) {
    let formatted_command = if command.ends_with("\n") {
        command.to_string()
    } else {
        command.to_owned() + "\n"
    };

    let result = state.resume(command_as_input(formatted_command));
    let output = output_as_string(result.get_output());
    return (result, output);
}

fn try_items(
    state: &WaitingForInputProgram,
    index: usize,
    dropped_indices: &mut HashSet<usize>,
) -> Option<String> {
    if index == ITEMS.len() {
        let (result, out) = send_command(state, "south");
        return match result {
            intcode::ProgramResult::WaitingForInput(_) => None,
            intcode::ProgramResult::Terminated(_) => Some(out),
        };
    }

    let result_if_keep = try_items(state, index + 1, dropped_indices);
    if result_if_keep.is_some() {
        return result_if_keep;
    }

    let (dropped, _) = send_command(state, &format!("drop {}", ITEMS[index]));
    if let intcode::ProgramResult::WaitingForInput(dropped) = dropped {
        let mut dropped_indices_2 = dropped_indices.clone();
        dropped_indices_2.insert(index);
        return try_items(&dropped, index + 1, &mut dropped_indices_2);
    }

    panic!("Unexpedted termination when dropping an item");
}

fn save(history: &Vec<String>) {
    fs::write(SAVE_FILENAME, history.join("")).unwrap();
    println!("Game has been saved");
}

fn bruteforce_items(state: &intcode::ProgramResult) -> Option<String> {
    match state {
        intcode::ProgramResult::WaitingForInput(waiting) => {
            try_items(waiting, 0, &mut HashSet::new())
        }
        _ => panic!("Cannot bruteforce items when program terminated"),
    }
}

fn load(program: &intcode::Program) -> (intcode::ProgramResult, Vec<String>, String) {
    let saved_state = fs::read_to_string(SAVE_FILENAME).unwrap();
    let saved_input = command_as_input(saved_state.clone());
    let state = intcode::run_program(program, saved_input);

    let history = saved_state
        .clone()
        .lines()
        .map(|line| line.to_owned() + "\n")
        .collect();

    let output = output_as_string(state.get_output());

    (state, history, output)
}

impl Day for Day25 {
    type Intermediate = intcode::Program;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        intcode::read_program(file)
    }

    fn part1(program: &Self::Intermediate) -> String {
        println!("Explore the map and collect these items:");
        println!("{:#?}", ITEMS);
        println!("Then go to the security checkpoint and use \"bruteforce\" command to try all items combos");
        println!("Press <Enter> to start");
        read_command();

        let mut state = intcode::run_program(program, vec![]);
        let mut output = output_as_string(state.get_output());
        let mut history = vec![];
        loop {
            println!("Robot says\n{}", output);

            let command = read_command();

            if command == "save\n" {
                save(&history);
                continue;
            } else if command == "load\n" {
                (state, history, output) = load(program);
                continue;
            } else if command == "bruteforce\n" {
                if let Some(out) = bruteforce_items(&state) {
                    return out;
                }
                println!("Didn't find proper combination of items.");
                continue;
            }

            history.push(command.clone());

            match state {
                intcode::ProgramResult::WaitingForInput(paused) => {
                    (state, output) = send_command(&paused, &command)
                }
                intcode::ProgramResult::Terminated(out) => return output_as_string(&out.output),
            }
        }
    }

    fn part2(_: &Self::Intermediate) -> String {
        "Nothing here".to_string()
    }
}
