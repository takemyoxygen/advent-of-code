open Base

let initial =
  Stdio.In_channel.read_all "./input/day7.txt"
  |> String.split ~on:',' |> List.map ~f:Int.of_string

let simple_cost pos crab = abs (crab - pos)

let progressive_cost pos crab =
  let dist = abs (crab - pos) in
  (2 + (dist - 1)) * dist / 2

let total_cost crab_cost crabs pos =
  List.fold crabs ~f:(fun acc crab -> acc + crab_cost pos crab) ~init:0

let solve cost =
  let start = List.min_elt initial ~compare:Int.compare |> Option.value_exn in
  let stop = List.max_elt initial ~compare:Int.compare |> Option.value_exn in
  Sequence.range ~stop:`inclusive start stop
  |> Sequence.map ~f:(total_cost cost initial)
  |> Sequence.min_elt ~compare:Int.compare
  |> Option.value_exn

let part1 () = solve simple_cost
let part2 () = solve progressive_cost
