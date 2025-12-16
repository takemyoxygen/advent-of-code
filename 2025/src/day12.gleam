import files
import gleam/dict
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import utils

fn parse_input(file) {
  let assert [regions, ..shapes] =
    files.read_file(file) |> string.split("\n\n") |> list.reverse

  let shapes =
    list.map(shapes, fn(shape) {
      let assert [num, ..lines] = string.split(shape, "\n")
      let num = string.replace(num, ":", "") |> utils.parse_or_panic
      #(num, lines)
    })
    |> dict.from_list

  let regions =
    string.split(regions, "\n")
    |> list.map(fn(line) {
      let assert [dims, quantities] = string.split(line, ": ")
      let assert [width, height] =
        string.split(dims, "x") |> list.map(utils.parse_or_panic)
      let quantities =
        string.split(quantities, " ") |> list.map(utils.parse_or_panic)
      #(#(width, height), quantities)
    })

  #(shapes, regions)
}

fn part1(shapes, regions) {
  let squares_per_shape =
    dict.map_values(shapes, fn(_, lines) {
      list.fold(lines, 0, fn(acc, line) {
        acc
        + {
          string.to_graphemes(line)
          |> list.count(fn(ch) {
            case ch {
              "#" -> True
              _ -> False
            }
          })
        }
      })
    })
  let not_feasible =
    list.count(regions, fn(region) {
      let #(#(width, height), quantities) = region
      let area = width * height
      let required_squares =
        list.index_fold(quantities, 0, fn(acc, quantity, index) {
          let assert Ok(squares) = dict.get(squares_per_shape, index)
          acc + squares * quantity
        })
      required_squares > area
    })
  list.length(regions) - not_feasible
}

pub fn solve(file) {
  let #(shapes, regions) = parse_input(file)
  let part1 = part1(shapes, regions) |> int.to_string |> option.Some
  #(part1, option.None)
}
