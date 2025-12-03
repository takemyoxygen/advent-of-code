import files
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import utils

fn parse_input(file: String) {
  files.read_file(file)
  |> string.split(",")
  |> list.map(fn(line) {
    let assert [start, finish] = string.split(line, "-")
      as { "unexpected line: " <> line }
    #(start, finish)
  })
}

fn num_digits(num, acc) {
  case num {
    num if num < 10 -> acc + 1
    _ -> num_digits(num / 10, acc + 1)
  }
}

fn repeat_number(num) {
  let digits = num_digits(num, 0)
  let assert Ok(mult) = int.power(10, int.to_float(digits))
  num * float.round(mult) + num
}

fn find_invalids(current, lo, hi, acc) {
  case repeat_number(current) {
    x if x < lo -> find_invalids(current + 1, lo, hi, acc)
    x if x > hi -> list.reverse(acc)
    x -> find_invalids(current + 1, lo, hi, [x, ..acc])
  }
}

pub fn find_invalid_ids(range) {
  let #(lo, hi) = range

  let start = case string.length(lo) {
    n if n % 2 == 0 -> string.slice(lo, 0, n / 2) |> utils.parse_or_panic
    n -> {
      let assert Ok(next_invalid_part) = int.power(10, int.to_float(n / 2))
      float.round(next_invalid_part)
    }
  }

  find_invalids(start, utils.parse_or_panic(lo), utils.parse_or_panic(hi), [])
}

fn part1(ranges) {
  ranges
  |> list.flat_map(find_invalid_ids)
  |> list.fold(0, fn(x, acc) { acc + x })
}

pub fn solve(file: String) {
  let ranges = parse_input(file)
  let part1 = part1(ranges) |> int.to_string |> option.Some
  #(part1, option.None)
}
