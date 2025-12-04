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

fn repeat_num(num, length, times) {
  let assert Ok(mult_10) = int.power(10, int.to_float(length))
  let mult_10 = float.round(mult_10)
  list.range(0, times - 1)
  |> list.fold(0, fn(acc, i) {
    let assert Ok(mult) = int.power(mult_10, int.to_float(i))
    acc + num * float.round(mult)
  })
}

fn all_nums_of_length(length) {
  let assert Ok(mult) = int.power(10, int.to_float(length - 1))
  let assert Ok(next_mult) = int.power(10, int.to_float(length))
  list.range(float.round(mult), float.round(next_mult) - 1)
}

fn all_repeats_with_length(base_length, times) {
  all_nums_of_length(base_length)
  |> list.map(fn(num) { repeat_num(num, base_length, times) })
}

fn repeats_of_count(max_length, count) {
  let lengths = list.range(1, max_length)
  list.flat_map(lengths, fn(length) {
    case length {
      _ if length % count == 0 -> {
        let base_length = length / count
        all_repeats_with_length(base_length, count)
      }
      _ -> []
    }
  })
}

fn all_repeats(max_length) {
  let lengths = list.range(2, max_length)
  list.flat_map(lengths, fn(count) { repeats_of_count(max_length, count) })
}

fn sum_within_ranges(nums, ranges) {
  ranges
  |> list.flat_map(fn(range) {
    let #(lo, hi) = range
    nums
    |> list.filter(fn(x) { lo <= x && x <= hi })
  })
  |> list.fold(0, fn(x, acc) { acc + x })
}

fn part1(max_length, ranges) {
  let repeats = repeats_of_count(max_length, 2) |> list.unique
  sum_within_ranges(repeats, ranges)
}

fn part2(max_length, ranges) {
  let repeats = all_repeats(max_length) |> list.unique
  sum_within_ranges(repeats, ranges)
}

pub fn solve(file: String) {
  let ranges = parse_input(file)
  let max_length =
    list.fold(ranges, 0, fn(acc, range) {
      let #(lo, hi) = range
      int.max(int.max(string.length(lo), string.length(hi)), acc)
    })
  let ranges =
    list.map(ranges, fn(range) {
      let #(lo, hi) = range
      #(utils.parse_or_panic(lo), utils.parse_or_panic(hi))
    })
  let part1 = part1(max_length, ranges) |> int.to_string |> option.Some
  let part2 = part2(max_length, ranges) |> int.to_string |> option.Some
  #(part1, part2)
}
