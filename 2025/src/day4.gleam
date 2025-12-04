import files
import gleam/int
import gleam/list
import gleam/option
import gleam/set
import gleam/string

fn parse_input(file) {
  files.read_file(file)
  |> string.split("\n")
  |> list.map(fn(line) { string.to_graphemes(line) })
  |> list.index_fold(set.new(), fn(rolls, line, y) {
    list.index_fold(line, rolls, fn(rolls, char, x) {
      case char {
        "@" -> set.insert(rolls, #(x, y))
        _ -> rolls
      }
    })
  })
}

fn adjacent_to(coord) {
  let #(x, y) = coord
  [
    #(x - 1, y),
    #(x + 1, y),
    #(x, y - 1),
    #(x, y + 1),
    #(x + 1, y + 1),
    #(x - 1, y - 1),
    #(x + 1, y - 1),
    #(x - 1, y + 1),
  ]
}

fn num_rolls_nearby(rolls, coord) {
  adjacent_to(coord)
  |> list.filter(fn(adj) { set.contains(rolls, adj) })
  |> list.length
}

fn accessible_rolls(rolls) {
  rolls
  |> set.to_list
  |> list.filter(fn(coord) { num_rolls_nearby(rolls, coord) < 4 })
}

fn part1(rolls) {
  accessible_rolls(rolls)
  |> list.length
}

fn part2_loop(rolls, acc) {
  let accessible_rolls = accessible_rolls(rolls)
  case list.length(accessible_rolls) {
    0 -> acc
    n -> {
      let acc = acc + n
      let rolls_left =
        list.fold(accessible_rolls, rolls, fn(rolls, coord) {
          set.delete(rolls, coord)
        })
      part2_loop(rolls_left, acc)
    }
  }
}

fn part2(rolls) {
  part2_loop(rolls, 0)
}

pub fn solve(file) {
  let rolls = parse_input(file)
  let part1 = part1(rolls) |> int.to_string |> option.Some
  let part2 = part2(rolls) |> int.to_string |> option.Some
  #(part1, part2)
}
