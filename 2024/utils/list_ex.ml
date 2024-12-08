open! Core

let minmax xs ~compare =
  let min = List.min_elt xs ~compare in
  let max = List.max_elt xs ~compare in
  Option.both min max

let all_pairs xs =
  let make_pairs_with x rest = List.map rest ~f:(fun y -> (x, y)) in
  let rec loop = function
    | [] | [ _ ] -> []
    | x :: rest -> make_pairs_with x rest @ loop rest
  in
  loop xs
