import files
import gleam/dict
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import glearray

fn parse_input(file) {
  file
  |> files.read_file
  |> string.split("\n")
  |> list.map(fn(line) { string.to_graphemes(line) |> glearray.from_list })
}

fn find_index(array, target) {
  list.range(0, glearray.length(array) - 1)
  |> list.find(fn(i) { glearray.get(array, i) == Ok(target) })
}

fn try_get(array, index) {
  let length = glearray.length(array)
  case index {
    x if x < 0 || x >= length -> option.None
    _ ->
      case glearray.get(array, index) {
        Ok(x) -> option.Some(x)
        Error(_) -> option.None
      }
  }
}

fn count_splits(lines, beams, splits_so_far) {
  case lines {
    [] -> splits_so_far
    [line, ..rest] -> {
      let #(splits_so_far, beams) =
        list.fold(beams, #(splits_so_far, []), fn(acc, beam) {
          let #(splits_so_far, new_beams) = acc
          case try_get(line, beam) {
            option.Some("^") -> #(splits_so_far + 1, [
              beam - 1,
              beam + 1,
              ..new_beams
            ])
            _ -> {
              #(splits_so_far, [beam, ..new_beams])
            }
          }
        })
      count_splits(rest, list.unique(beams), splits_so_far)
    }
  }
}

fn part1(lines, start) {
  count_splits(lines, [start], 0)
}

fn count_timelines(lines, beam, y, memo) {
  case dict.get(memo, #(beam, y)) {
    Ok(memoized) -> #(memoized, memo)
    _ ->
      case lines {
        [] -> #(1, memo)
        [line, ..rest] -> {
          let #(result, memo) = case try_get(line, beam) {
            option.Some("^") -> {
              let #(timelines_left, memo) =
                count_timelines(rest, beam - 1, y + 1, memo)
              let #(timelines_right, memo) =
                count_timelines(rest, beam + 1, y + 1, memo)
              #(timelines_left + timelines_right, memo)
            }
            _ -> count_timelines(rest, beam, y + 1, memo)
          }
          let memo = dict.insert(memo, #(beam, y), result)
          #(result, memo)
        }
      }
  }
}

fn part2(lines, start) {
  let memo = dict.new()
  count_timelines(lines, start, 1, memo).0
}

pub fn solve(file) {
  let grid = parse_input(file)
  let assert [first_line, ..rest] = grid
  let assert Ok(start) = find_index(first_line, "S")
  let part1 = part1(rest, start) |> int.to_string |> option.Some
  let part2 = part2(rest, start) |> int.to_string |> option.Some
  #(part1, part2)
}
