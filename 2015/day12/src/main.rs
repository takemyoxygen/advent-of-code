extern crate serde_json;

use std::io;
use std::io::Read;
use std::collections::BTreeMap;
use serde_json::Value;

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    try!(io::stdin().read_to_string(&mut buffer));
    Ok(buffer.trim().to_string())
}

fn contains_forbidden_value(object: &BTreeMap<String, Value>, forbidden: &Option<&str>) -> bool {
    match forbidden {
        &Some(ref x) => {
            let forbidden = Value::String(x.to_string());
            object.values().any(|x| *x == forbidden)
        },
        _ => false
    }
}

fn find_sum(json: &Value, forbidden: &Option<&str>) -> i64 {
    match json {
        &Value::I64(x) => x,
        &Value::U64(x) => x as i64,
        &Value::F64(x) => x as i64,
        &Value::Array(ref xs) => xs.iter().map(|x| find_sum(x, forbidden)).fold(0, |acc, x| acc + x),
        &Value::Object(ref obj) if !contains_forbidden_value(obj, forbidden) =>
            obj.values().map(|x| find_sum(x, forbidden)).fold(0, |acc, x| acc + x),
        _ => 0
    }
}

fn main() {
    let input = read_input().unwrap();
    let result = serde_json::from_str::<Value>(&input);

    if result.is_err(){
        println!("Parsing failed: {:?}", result);
    } else {
        let json = result.unwrap();

        let sum = find_sum(&json, &None);
        println!("Sum: {}", sum);

        let sum_without_red = find_sum(&json, &Some("red"));
        println!("Sum without red: {}", sum_without_red);
    }
}
