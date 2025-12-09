import dsu
import files
import gleam/int
import gleam/list
import gleam/option
import gleam/pair
import gleam/string
import utils

fn parse_input(file) {
  files.read_file(file)
  |> string.split("\n")
  |> list.map(fn(line) {
    let assert [x, y, z] = string.split(line, ",")
    #(utils.parse_or_panic(x), utils.parse_or_panic(y), utils.parse_or_panic(z))
  })
}

fn distance(p1, p2) {
  let #(x1, y1, z1) = p1
  let #(x2, y2, z2) = p2
  let square = fn(x) { x * x }
  square(x1 - x2) + square(y1 - y2) + square(z1 - z2)
}

fn closest_pairs(points) {
  list.combination_pairs(points)
  |> list.sort(fn(pair1, pair2) {
    int.compare(distance(pair1.0, pair1.1), distance(pair2.0, pair2.1))
  })
}

fn part1(points) {
  let points_to_take = case list.length(points) {
    n if n < 100 -> 10
    _ -> 1000
  }
  closest_pairs(points)
  |> list.take(points_to_take)
  |> list.fold(dsu.of_list(points), fn(dsu, pair) {
    let assert Ok(dsu) = dsu.union(dsu, pair.0, pair.1)
    dsu
  })
  |> dsu.roots
  |> list.map(pair.second)
  |> list.sort(fn(a, b) { int.compare(b, a) })
  |> list.take(3)
  |> list.fold(1, fn(a, b) { a * b })
}

fn connect_until_together(dsu, pairs) {
  let assert [#(p1, p2), ..rest] = pairs
  let assert Ok(dsu) = dsu.union(dsu, p1, p2)
  case dsu.roots_count(dsu) {
    1 -> #(p1, p2)
    _ -> connect_until_together(dsu, rest)
  }
}

fn part2(points) {
  let dsu = dsu.of_list(points)
  let pairs = closest_pairs(points)
  let #(#(x1, _, _), #(x2, _, _)) = connect_until_together(dsu, pairs)
  x1 * x2
}

pub fn solve(file) {
  let points = parse_input(file)
  let part1 = part1(points) |> int.to_string |> option.Some
  let part2 = part2(points) |> int.to_string |> option.Some
  #(part1, part2)
}
