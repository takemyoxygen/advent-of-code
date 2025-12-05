import files
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import utils

fn parse_input(file) {
  let content = files.read_file(file)
  let assert [ranges, ids] = string.split(content, "\n\n")
  let ids =
    string.split(ids, "\n")
    |> list.map(utils.parse_or_panic)
  let ranges =
    string.split(ranges, "\n")
    |> list.map(fn(range) {
      let assert [lo, hi] = string.split(range, "-")
      #(utils.parse_or_panic(lo), utils.parse_or_panic(hi))
    })
  #(ranges, ids)
}

fn part1(ranges, ids) {
  list.count(ids, fn(id) {
    list.any(ranges, fn(range) {
      let #(lo, hi) = range
      id >= lo && id <= hi
    })
  })
}

fn part2(ranges: List(#(Int, Int))) {
  let ranges = list.sort(ranges, fn(r1, r2) { int.compare(r1.0, r2.0) })
  let assert [first, ..] = ranges
  list.fold(ranges, #(0, first.0 - 1), fn(acc, range) {
    let #(cnt, prev) = acc
    let #(lo, hi) = range
    let start_from = int.max(lo, prev + 1)
    let delta = hi - start_from + 1
    let cnt = case delta {
      x if x < 0 -> cnt
      x -> cnt + x
    }
    let next = int.max(prev, hi)
    #(cnt, next)
  }).0
}

pub fn solve(file) {
  let #(ranges, ids) = parse_input(file)
  let part1 = part1(ranges, ids) |> int.to_string |> option.Some
  let part2 = part2(ranges) |> int.to_string |> option.Some
  #(part1, part2)
}
