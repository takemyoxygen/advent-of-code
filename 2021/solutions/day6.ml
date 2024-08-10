open Base
open Core

let initial =
  Stdio.In_channel.read_all "./input/day6.txt"
  |> String.split ~on:',' |> List.map ~f:Int.of_string

let fishes_after =
  let memo =
    Hashtbl.create
      (module struct
        type t = int * int

        include Tuple.Hashable (Int) (Int)
      end)
  in
  let rec loop days_left state =
    Hashtbl.find_or_add memo (days_left, state) ~default:(fun () ->
        match (state, days_left) with
        | _, 0 -> 1
        | 0, _ -> loop (days_left - 1) 6 + loop (days_left - 1) 8
        | _ -> loop (days_left - 1) (state - 1))
  in
  loop

let solve days =
  initial
  |> List.map ~f:(fishes_after days)
  |> List.reduce ~f:( + ) |> Option.value ~default:0

let part1 = solve 80
let part2 = solve 256
