open Base
open Core

let input = Stdio.In_channel.read_lines "./input/day10.txt"
let open_to_close = [ ('(', ')'); ('[', ']'); ('{', '}'); ('<', '>') ]
let close_to_open = List.Assoc.inverse open_to_close
let is_opening ch = List.Assoc.mem open_to_close ch ~equal:Char.equal

type check_result = Valid | Incomplete of char list | Corrupted of char

let matching_opening op = List.Assoc.find_exn close_to_open op ~equal:Char.equal
let matching_closing op = List.Assoc.find_exn open_to_close op ~equal:Char.equal

let points = function
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | ch -> failwith (sprintf "Unexpected character - %c" ch)

let check_line line =
  let chars = String.to_list line in
  let rec loop openings chars =
    match (chars, openings) with
    | [], [] -> Valid
    | [], _ -> Incomplete openings
    | ch :: rest, _ when is_opening ch -> loop (ch :: openings) rest
    | ch :: _, [] -> Corrupted ch
    | ch :: rest, op :: rest_op when Char.equal (matching_opening ch) op ->
        loop rest_op rest
    | ch :: _, _ -> Corrupted ch
  in
  loop [] chars

let completion_score remainings =
  remainings
  |> List.map ~f:matching_closing
  |> List.map ~f:(function
       | ')' -> 1
       | ']' -> 2
       | '}' -> 3
       | '>' -> 4
       | _ -> assert false)
  |> List.fold ~init:0 ~f:(fun acc v -> (acc * 5) + v)

let part1 () =
  input |> List.map ~f:check_line
  |> List.filter_map ~f:(function Corrupted ch -> Some ch | _ -> None)
  |> List.map ~f:points |> List.reduce_exn ~f:( + )

let part2 () =
  let scores =
    input |> List.map ~f:check_line
    |> List.filter_map ~f:(function Incomplete rem -> Some rem | _ -> None)
    |> List.map ~f:completion_score
    |> List.sort ~compare:Int.compare
    |> Array.of_list
  in
  scores.(Array.length scores / 2)
