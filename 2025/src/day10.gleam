import child_process
import files
import gleam/deque
import gleam/function
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/regexp
import gleam/set
import gleam/string
import utils

fn extract_from_match(match: regexp.Match) {
  let assert [lights, buttons, joltage] =
    match.submatches
    |> list.filter_map(fn(sub) {
      case sub {
        option.None -> Error(Nil)
        option.Some(s) -> Ok(s)
      }
    })
  let lights =
    string.to_graphemes(lights)
    |> list.index_map(fn(ch, idx) {
      case ch {
        "#" -> Ok(idx)
        _ -> Error(Nil)
      }
    })
    |> list.filter_map(function.identity)
    |> set.from_list

  let buttons =
    buttons
    |> string.replace("(", "")
    |> string.replace(")", "")
    |> string.split(" ")
    |> list.filter(fn(x) { !string.is_empty(x) })
    |> list.map(fn(group) {
      string.split(group, ",")
      |> list.map(utils.parse_or_panic)
      |> set.from_list
    })

  let joltage =
    string.split(joltage, ",")
    |> list.map(utils.parse_or_panic)

  #(lights, buttons, joltage)
}

fn parse_input(input) {
  let assert Ok(re) = regexp.from_string("\\[(.*)\\](.*){(.*)}")
  let lines = files.read_file(input) |> string.split("\n")
  list.map(lines, fn(line) { regexp.scan(re, line) })
  |> list.map(fn(matches) {
    let assert [match] = matches
    extract_from_match(match)
  })
}

fn press_btn(lights, button) {
  set.fold(button, lights, fn(acc, light) {
    case set.contains(acc, light) {
      True -> set.delete(acc, light)
      False -> set.insert(acc, light)
    }
  })
}

fn bfs(target, buttons, visited, queue) {
  case deque.pop_front(queue) {
    Error(Nil) -> panic as "No way"
    Ok(#(#(lights, presses), _)) if lights == target -> presses
    Ok(#(#(lights, presses), queue)) -> {
      case set.contains(visited, lights) {
        True -> bfs(target, buttons, visited, queue)
        False -> {
          let next =
            list.map(buttons, fn(btn) { press_btn(lights, btn) })
            |> list.filter(fn(n) { !set.contains(visited, n) })

          let queue =
            list.fold(next, queue, fn(queue, n) {
              deque.push_back(queue, #(n, presses + 1))
            })
          bfs(target, buttons, set.insert(visited, lights), queue)
        }
      }
    }
  }
}

fn part1(input) {
  list.map(input, fn(input_row) {
    let #(target, buttons, _) = input_row
    bfs(target, buttons, set.new(), deque.from_list([#(set.new(), 0)]))
  })
  |> list.fold(0, fn(acc, x) { acc + x })
}

// Basically, calls "day10.py", should be run in the virtual env from .venv folder
fn part2(input) {
  let filename = "./input/day10-for-z3.json"
  let json_input =
    json.array(input, fn(row) {
      let #(_, buttons, joltage) = row
      let buttons =
        json.array(buttons, fn(btn) { set.to_list(btn) |> json.array(json.int) })
      let joltage = json.array(joltage, json.int)
      json.object([#("buttons", buttons), #("target", joltage)])
    })

  files.write_file(filename, json.to_string(json_input))

  let assert Ok(python_output) =
    child_process.new_with_path("python3")
    |> child_process.args(["./src/day10.py", filename])
    |> child_process.run()

  python_output.output |> string.trim() |> utils.parse_or_panic
}

pub fn solve(input) {
  let input = parse_input(input)
  let part1 = part1(input) |> int.to_string |> option.Some
  let part2 = part2(input) |> int.to_string |> option.Some
  #(part1, part2)
}
