import files
import gleam/int
import gleam/list
import gleam/option
import gleam/pair
import gleam/string
import utils

type Point =
  #(Int, Int)

fn parse_input(file) {
  files.read_file(file)
  |> string.split("\n")
  |> list.map(fn(line) {
    let assert [x, y] = string.split(line, ",")
    #(utils.parse_or_panic(x), utils.parse_or_panic(y))
  })
}

fn area(p1: #(Int, Int), p2: #(Int, Int)) {
  let dx = int.absolute_value(p1.0 - p2.0) + 1
  let dy = int.absolute_value(p1.1 - p2.1) + 1
  dx * dy
}

fn is_vertical(segment: #(Point, Point)) {
  let #(p1, p2) = segment
  p1.0 == p2.0
}

fn min_max(rect, f) {
  let #(p1, p2) = rect
  #(int.min(f(p1), f(p2)), int.max(f(p1), f(p2)))
}

pub fn to_segments(points) {
  case points {
    [] | [_] -> {
      []
    }
    [head, ..tail] -> {
      let #(last, segments) =
        list.fold(tail, #(head, []), fn(acc, pt) {
          let #(prev, segments) = acc
          let segments = [#(prev, pt), ..segments]
          #(pt, segments)
        })
      [#(last, head), ..segments] |> list.reverse
    }
  }
}

pub fn intersects(rect, segment: #(Point, Point)) {
  let #(min_x, max_x) = min_max(rect, pair.first)
  let #(min_y, max_y) = min_max(rect, pair.second)
  let #(s1, s2) = segment

  case is_vertical(segment) {
    True -> {
      let x = s1.0
      min_x < x
      && x < max_x
      && !{
        { s1.1 >= max_y && s2.1 >= max_y } || { s1.1 <= min_y && s2.1 <= min_y }
      }
    }
    False -> {
      let y = s1.1
      min_y < y
      && y < max_y
      && !{
        { s1.0 >= max_x && s2.0 >= max_x } || { s1.0 <= min_x && s2.0 <= min_x }
      }
    }
  }
}

fn part2(points) {
  let segments = to_segments(points)
  let assert [biggest, ..] =
    list.combination_pairs(points)
    |> list.filter(fn(rect) {
      !list.any(segments, fn(seg) { intersects(rect, seg) })
    })
    |> list.map(fn(rect) { #(rect, area(rect.0, rect.1)) })
    |> list.sort(fn(pair1, pair2) { int.compare(pair2.1, pair1.1) })
  biggest.1
}

fn part1(points) {
  list.combination_pairs(points)
  |> list.map(fn(pair) { area(pair.0, pair.1) })
  |> list.fold(0, int.max)
}

pub fn solve(file) {
  let points = parse_input(file)
  let part1 = part1(points) |> int.to_string |> option.Some
  let part2 = part2(points) |> int.to_string |> option.Some
  #(part1, part2)
}
