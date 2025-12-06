import files
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import utils

fn zip_n(lists, acc) {
  case list.any(lists, list.is_empty) {
    True -> list.reverse(acc)
    False -> {
      let #(heads, tails) =
        list.fold(lists, #([], []), fn(acc, lst) {
          let #(heads, tails) = acc
          let assert [head, ..tail] = lst
          #([head, ..heads], [tail, ..tails])
        })
      zip_n(tails, [heads, ..acc])
    }
  }
}

fn parse_horizontal(lines) {
  let tokens =
    list.map(lines, fn(line) {
      string.split(line, " ") |> list.filter(fn(x) { !string.is_empty(x) })
    })

  let assert [operators, ..numbers] = list.reverse(tokens)
  let numbers =
    list.map(numbers, fn(row) {
      list.map(row, fn(num) { utils.parse_or_panic(num) })
    })
  let grouped = zip_n(numbers, [])
  list.zip(operators, grouped)
}

fn trim_operator(s) {
  let assert Ok(operator) = string.last(s)
  let operand = string.drop_end(s, 1) |> string.trim
  #(operator, operand)
}

fn parse_cols(lines) {
  lines
  |> list.map(fn(line) { string.to_graphemes(line) })
  |> list.transpose
  |> list.map(fn(symbols) { string.join(symbols, "") })
  |> list.map(string.trim)
}

fn calculate(operator, operands) {
  let #(f, init) = case operator {
    "+" -> #(fn(a, b) { a + b }, 0)
    "*" -> #(fn(a, b) { a * b }, 1)
    _ -> panic as { "Unexpected operator: " <> operator }
  }
  list.fold(operands, init, f)
}

fn solve_problems(tokens, acc) {
  case tokens {
    [] -> acc
    tokens -> {
      let #(current_problem, rest) =
        list.split_while(tokens, fn(token) { !string.is_empty(token) })
      let rest = case rest {
        ["", ..tail] -> tail
        [] -> []
        _ ->
          panic as {
            "Expected empty string separator, got: " <> string.join(rest, ", ")
          }
      }
      let assert [first, ..others] = current_problem
      let #(op, operand_str) = trim_operator(first)
      let operands =
        [operand_str, ..others] |> list.map(fn(x) { utils.parse_or_panic(x) })
      let result = calculate(op, operands)
      let acc = result + acc
      solve_problems(rest, acc)
    }
  }
}

fn part2(file) {
  let problem_tokens = parse_cols(file)
  solve_problems(problem_tokens, 0)
}

fn part1(lines) {
  let input = parse_horizontal(lines)
  list.map(input, fn(problem) {
    let #(op, nums) = problem
    calculate(op, nums)
  })
  |> list.fold(0, fn(a, b) { a + b })
}

pub fn solve(file) {
  let lines = files.read_file(file) |> string.split("\n")
  let part1 = part1(lines) |> int.to_string |> option.Some
  let part2 = part2(lines) |> int.to_string |> option.Some
  #(part1, part2)
}
