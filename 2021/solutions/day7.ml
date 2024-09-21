open Base

let simple_cost pos crab = abs (crab - pos)

let progressive_cost pos crab =
  let dist = abs (crab - pos) in
  (2 + (dist - 1)) * dist / 2

let total_cost crab_cost crabs pos =
  List.fold crabs ~f:(fun acc crab -> acc + crab_cost pos crab) ~init:0

let solve_part initial cost =
  let start = List.min_elt initial ~compare:Int.compare |> Option.value_exn in
  let stop = List.max_elt initial ~compare:Int.compare |> Option.value_exn in
  Sequence.range ~stop:`inclusive start stop
  |> Sequence.map ~f:(total_cost cost initial)
  |> Sequence.min_elt ~compare:Int.compare
  |> Option.value_exn

let solve filename =
  let initial =
    Stdio.In_channel.read_all filename
    |> String.split ~on:',' |> List.map ~f:Int.of_string
  in
  let part1 = solve_part initial simple_cost in
  let part2 = solve_part initial progressive_cost in
  (Some (Int.to_string part1), Some (Int.to_string part2))
