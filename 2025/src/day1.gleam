import files
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import utils

type Instruction {
  L(distance: Int)
  R(distance: Int)
}


fn parse_input(lines) {
  list.map(lines, fn(line) {
    case line {
      "L" <> dist -> L(utils.parse_or_panic(dist))
      "R" <> dist -> R(utils.parse_or_panic(dist))
      _ -> panic as { "Unexpected input line: " <> line }
    }
  })
}

fn part1(instructions: List(Instruction)) {
  let #(_, cnt) =
    list.fold(instructions, #(50, 0), fn(acc, instr) {
      let #(pos, cnt) = acc
      let delta = case instr {
        L(distance) -> -distance
        R(distance) -> distance
      }
      let next_pos = case { pos + delta } % 100 {
        pos if pos < 0 -> pos + 100
        pos -> pos
      }
      let next_cnt = case pos {
        0 -> cnt + 1
        _ -> cnt
      }
      #(next_pos, next_cnt)
    })
  cnt
}

fn part2(instructions: List(Instruction)) {
  let #(_, cnt) =
    list.fold(instructions, #(50, 0), fn(acc, instr) {
      let #(pos, cnt) = acc
      let delta = case instr {
        L(distance) -> -distance
        R(distance) -> distance
      }
      let next = pos + delta
      let zeros = case next {
        0 -> 1
        _ if next > 0 -> next / 100
        _ -> {
          let carry = case pos {
            0 -> 0
            _ -> 1
          }
          carry + -1 * next / 100
        }
      }
      let next_pos = case next % 100 {
        pos if pos < 0 -> pos + 100
        pos -> pos
      }
      let next_cnt = cnt + zeros
      #(next_pos, next_cnt)
    })
  cnt
}

pub fn solve(filename: String) {
  let lines = string.split(files.read_file(filename), "\n")
  let instructions = parse_input(lines)
  let part1 = part1(instructions) |> int.to_string |> option.Some
  let part2 = part2(instructions) |> int.to_string |> option.Some
  #(part1, part2)
}
