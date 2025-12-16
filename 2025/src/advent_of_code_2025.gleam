import argv
import day1
import day10
import day11
import day12
import day2
import day3
import day4
import day5
import day6
import day7
import day8
import day9
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option

pub fn main() -> Nil {
  let days =
    [
      day1.solve,
      day2.solve,
      day3.solve,
      day4.solve,
      day5.solve,
      day6.solve,
      day7.solve,
      day8.solve,
      day9.solve,
      day10.solve,
      day11.solve,
      day12.solve,
    ]
    |> list.index_map(fn(x, i) { #(i + 1, x) })
    |> dict.from_list
  case argv.load().arguments {
    ["day", day_str, input_file] ->
      case int.parse(day_str) {
        Ok(day) -> {
          io.println("Running Advent of Code 2025, Day " <> day_str)
          let assert Ok(solve) = dict.get(days, day)
            as "Did you solve this day?"
          let #(part1, part2) = solve(input_file)
          io.println("Part 1: " <> option.unwrap(part1, "N/A"))
          io.println("Part 2: " <> option.unwrap(part2, "N/A"))
        }
        Error(_) -> io.println("Please provide a valid day number.")
      }
    _args ->
      io.println("Unexpected args. Expected usage: run 12 input/day12.txt")
  }
}
