use std::collections::HashMap;
use std::str::FromStr;

type Register = char;
type Computer = HashMap<Register, usize>;

#[derive(Debug)]
enum Instruction {
    Half(Register),
    Triple(Register),
    Increment(Register),
    Jump(i32),
    JumpIfEven(Register, i32),
    JumpIfOne(Register, i32)
}

fn execute_instruction(computer: &mut Computer, instructions: &Vec<Instruction>, head: i32) -> i32 {
    match instructions[head as usize] {
        Instruction::Half(r) => {
            let entry = computer.entry(r).or_insert(0);
            *entry /= 2;
            head + 1
        },
        Instruction::Triple(r) => {
            let entry = computer.entry(r).or_insert(0);
            *entry *= 3;
            head + 1
        },
        Instruction::Increment(r) => {
            let entry = computer.entry(r).or_insert(0);
            *entry += 1;
            head + 1
        },
        Instruction::Jump(offset) => head + offset,
        Instruction::JumpIfEven(r, offset) => {
            if computer.get(&r).unwrap_or(&0) % 2 == 0 { head + offset }
            else { head + 1 }
        },
        Instruction::JumpIfOne(r, offset) => {
            if *computer.get(&r).unwrap_or(&0) == 1 { head + offset }
            else { head + 1 }
        }
    }
}

fn execute_program(computer: &mut Computer, program: &Vec<Instruction>) {
    let mut head: i32 = 0;
    loop {
        if head >= 0 && head < program.len() as i32{
            head = execute_instruction(computer, &program, head);
        } else {
            break;
        }
    }
}

fn parse(input: &str) -> Vec<Instruction> {
    input
        .lines()
        .map(|line|{
            let tokens: Vec<_> = line.split_whitespace().collect();
            let register = |i: usize| tokens[i].chars().next().unwrap();
            let offset = |i: usize| i32::from_str(tokens[i].replace("+", "").as_ref()).unwrap();
            match tokens[0]{
                "hlf" => Instruction::Half(register(1)),
                "tpl" => Instruction::Triple(register(1)),
                "inc" => Instruction::Increment(register(1)),
                "jmp" => Instruction::Jump(offset(1)),
                "jie" => Instruction::JumpIfEven(register(1), offset(2)),
                "jio" => Instruction::JumpIfOne(register(1), offset(2)),
                _ => panic!("Instruction \"{}\" is not supported", line)
            }
        })
        .collect()
}

fn main() {
    let input = include_str!("../input.txt");
    let program = parse(input);

    let mut computer = Computer::new();
    execute_program(&mut computer, &program);
    println!("Part 1: computer in the end: {:?}", computer);

    let mut computer = Computer::new();
    computer.insert('a', 1);
    execute_program(&mut computer, &program);
    println!("Part 2: computer in the end: {:?}", computer);
}
