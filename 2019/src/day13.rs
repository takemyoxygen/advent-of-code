use crate::{coord, intcode, solver::Day};
use im::HashMap;
use itertools::Itertools;
use std::path::Path;

pub struct Day13;

struct GameState {
    tiles: HashMap<coord::Vec2D, char>,
    score: usize,
}

fn tiles_string(tiles: &HashMap<coord::Vec2D, char>) -> String {
    let mut output = String::new();
    let max_x = tiles.keys().map(|p| p.x).max().unwrap();
    let max_y = tiles.keys().map(|p| p.y).max().unwrap();

    for y in 0..=max_y {
        for x in 0..=max_x {
            let char = tiles.get(&coord::Vec2D { x, y }).unwrap_or(&' ');
            output.push(*char);
        }
        output.push('\n');
    }

    output
}

fn process_output(game: &mut GameState, output: &Vec<i64>) {
    for chunk in output.chunks(3) {
        let (x, y, t) = (chunk[0], chunk[1], chunk[2]);
        if x == -1 && y == 0 {
            game.score = t as usize;
        } else {
            let tile = match t {
                0 => ' ',
                1 => '|',
                2 => '#',
                3 => '-',
                4 => 'o',
                _ => panic!("Invalid tile ID: {}", t),
            };
            game.tiles.insert(
                coord::Vec2D {
                    x: x as i32,
                    y: y as i32,
                },
                tile,
            );
        }
    }
}

fn save_game(inputs_history: &Vec<i64>) {
    std::fs::write("./saved-game", inputs_history.iter().join(",")).unwrap()
}

fn load_game() -> Vec<i64> {
    if Path::exists(Path::new("./saved-game")) {
        std::fs::read_to_string("./saved-game")
            .unwrap()
            .trim()
            .split(",")
            .map(|token| str::parse::<i64>(token).unwrap())
            .collect()
    } else {
        vec![]
    }
}

fn play_interactively(program: &intcode::Program) -> String {
    let mut game = GameState {
        tiles: HashMap::new(),
        score: 0,
    };
    let mut inputs_history = load_game();
    let mut program_state = intcode::run_program(program, inputs_history.clone());

    loop {
        process_output(&mut game, program_state.get_output());

        match program_state {
            intcode::ProgramResult::WaitingForInput(waiting) => {
                println!(
                    "\n\nScore: {}\n{}\nYour move!",
                    game.score,
                    tiles_string(&game.tiles)
                );

                let mut user_input = String::new();
                loop {
                    std::io::stdin().read_line(&mut user_input).unwrap();
                    if user_input.trim() == "s" {
                        save_game(&inputs_history);
                        user_input.clear();
                    } else {
                        break;
                    }
                }

                let program_input = match user_input.trim() {
                    "a" => -1,
                    "d" => 1,
                    "" => 0,
                    _ => panic!("Invalid input: {}", user_input),
                };

                inputs_history.push(program_input);
                program_state = waiting.resume(vec![program_input]);
            }
            _ => return game.score.to_string(),
        }
    }
}

impl Day for Day13 {
    type Intermediate = intcode::Program;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        intcode::read_program(file)
    }

    fn part1(input: &Self::Intermediate) -> String {
        intcode::run_program(input, Vec::new())
            .get_output()
            .iter()
            .chunks(3)
            .into_iter()
            .map(|chunk| chunk.collect::<Vec<&i64>>())
            .filter(|chunk| *chunk[2] == 2)
            .count()
            .to_string()
    }

    fn part2(input: &Self::Intermediate) -> String {
        let mut program = input.clone();
        program[0] = 2;

        play_interactively(&program)
    }
}
