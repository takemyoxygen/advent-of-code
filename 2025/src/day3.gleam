import files
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import utils

fn max_in_list_loop(acc, nums_left, count_left, how_many_to_keep) {
  let #(max_so_far, _) = acc
  case nums_left, count_left {
    [], _ -> acc
    _, left if left == how_many_to_keep -> acc
    [head, ..tail], _ if head > max_so_far -> {
      max_in_list_loop(#(head, tail), tail, count_left - 1, how_many_to_keep)
    }
    [_, ..tail], _ -> {
      max_in_list_loop(acc, tail, count_left - 1, how_many_to_keep)
    }
  }
}

pub fn max_in_list(nums, how_many_to_keep how_many_to_keep) {
  max_in_list_loop(#(0, nums), nums, list.length(nums), how_many_to_keep)
}

fn parse_input(file) {
  file
  |> files.read_file
  |> string.split("\n")
  |> list.map(fn(line) {
    line |> string.to_graphemes |> list.map(utils.parse_or_panic)
  })
}

fn part1(banks) {
  list.map(banks, fn(bank) {
    let #(first, rest) = max_in_list(bank, how_many_to_keep: 1)
    let #(second, _) = max_in_list(rest, how_many_to_keep: 0)
    first * 10 + second
  })
  |> list.fold(0, fn(x, acc) { acc + x })
}

fn part2(banks) {
  let digits = list.range(0, 11) |> list.reverse
  list.map(banks, fn(bank) {
    let #(largest, _) =
      digits
      |> list.fold(#([], bank), fn(acc, keep) {
        let #(largest, left) = acc
        let #(next, rest) = max_in_list(left, how_many_to_keep: keep)
        #([next, ..largest], rest)
      })
    list.reverse(largest)
  })
  |> list.map(fn(digits) {
    list.fold(digits, 0, fn(acc, digit) { acc * 10 + digit })
  })
  |> list.fold(0, fn(x, acc) { acc + x })
}

pub fn solve(file) {
  let input = parse_input(file)
  let part1 = part1(input) |> int.to_string |> option.Some
  let part2 = part2(input) |> int.to_string |> option.Some
  #(part1, part2)
}
