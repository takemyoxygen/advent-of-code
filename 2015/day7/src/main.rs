use std::collections::HashMap;
use std::str::FromStr;
use std::io;
use std::io::Read;

type Wire = String;

type Signal = u16;

type Shift = u16;

enum Input {
    Const(Signal),
    Wire(Wire)
}

enum Connector {
    Direct(Input), // x -> z or 123 -> z
    And(Input, Input),
    Or(Input, Input),
    LShift(Input, Shift),
    RShift(Input, Shift),
    Not(Input)
}

type Circuit = HashMap<Wire, Connector>;
type Cache = HashMap<Wire, Signal>;

fn evaluate_input_signal(input: &Input, circuit: &Circuit, cache: &mut Cache) -> Signal {
    match *input {
        Input::Const(signal) => signal,
        Input::Wire(ref wire) => evaluate_wire_signal(&wire, circuit, cache)
    }
}

fn resolve_signal(wire: &Wire, circuit: &Circuit, cache: &mut Cache) -> Signal {
    let mut eval = |input| evaluate_input_signal(input, circuit, cache);

    match *circuit.get(wire).unwrap() {
        Connector::Direct(ref input) => eval(input),
        Connector::And(ref left, ref right) => eval(left) & eval(right),
        Connector::Or(ref left, ref right) => eval(left) | eval(right),
        Connector::Not(ref input) => !eval(input),
        Connector::LShift(ref input, shift) => eval(input) << shift,
        Connector::RShift(ref input, shift) => eval(input) >> shift
    }
}

fn evaluate_wire_signal(wire: &Wire, circuit: &Circuit, cache: &mut Cache) -> Signal {
    if cache.contains_key(wire) {
         *cache.get(wire).unwrap()
    }
    else {
        let signal = resolve_signal(wire, circuit, cache);
        cache.insert(wire.clone(), signal.clone());
        signal

    }
}

fn parse_input(string: &str) -> Input {
    match Signal::from_str(string) {
        Ok(signal) => Input::Const(signal),
        Err(_) => Input::Wire(string.to_string())
    }
}

fn parse_connector(string: &str) -> Connector {
    let tokens: Vec<_> = string.split_whitespace().collect();

    match tokens.len() {
        1 => Connector::Direct(parse_input(tokens[0])),
        2 if tokens[0] == "NOT" => Connector::Not(parse_input(tokens[1])),
        3 => match tokens[1] {
            "AND" => Connector::And(parse_input(tokens[0]), parse_input(tokens[2])),
            "OR" => Connector::Or(parse_input(tokens[0]), parse_input(tokens[2])),
            "LSHIFT" => Connector::LShift(parse_input(tokens[0]), Shift::from_str(tokens[2]).unwrap()),
            "RSHIFT" => Connector::RShift(parse_input(tokens[0]), Shift::from_str(tokens[2]).unwrap()),
            _ => panic!("Not supported connector: {}", string)
        },
        _ => panic!("Not supported connector: {}", string)
    }
}

fn parse(string: &str) -> (Connector, Wire) {
    let tokens: Vec<_> = string.split("->").collect();
    let destination = tokens[1].trim().to_string();

    (parse_connector(tokens[0]), destination)
}

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

fn override_signal(wire: &Wire, signal: Signal, circuit: &mut Circuit) {
    let target = circuit.get_mut(wire).unwrap();
    *target = Connector::Direct(Input::Const(signal));
}

fn main() {
    let mut circuit = HashMap::new();
    let mut cache = HashMap::new();
    let input = read_input().unwrap();
    let instructions = input.lines().map(parse);

    for (connector, wire) in instructions {
        circuit.insert(wire, connector);
    }

    let a = &"a".to_string();
    let signal = evaluate_wire_signal(a, &circuit, &mut cache);
    println!("a: {}", signal);

    let b = &"b".to_string();
    override_signal(b, signal, &mut circuit);

    cache.clear();
    let signal = evaluate_wire_signal(a, &circuit, &mut cache);
    println!("a again: {}", signal);
}
