open Core

let read_file filename =
  Stdio.In_channel.read_lines filename
  |> List.map ~f:(fun line ->
         String.split line ~on:' ' |> List.map ~f:Int.of_string)

let all_diffs_between xs low high =
  let rec loop = function
    | [] | [ _ ] -> true
    | curr :: next :: rest ->
        Int.between (next - curr) ~low ~high && loop (next :: rest)
  in
  loop xs

let get_diff_range levels =
  match levels with
  | f :: s :: _ when f = s -> None
  | f :: s :: _ when s > f -> Some (1, 3)
  | _ -> Some (-3, -1)

let are_safe levels =
  get_diff_range levels
  |> Option.map ~f:(fun (l, h) -> all_diffs_between levels l h)
  |> Option.value ~default:false

let all_diffs_between_relaxed xs low high =
  let rec loop prev = function
    | xs when List.length xs < 2 -> true
    | curr :: next :: rest ->
        let with_prev xs = match prev with Some x -> x :: xs | None -> xs in
        (Int.between (next - curr) ~low ~high && loop (Some curr) (next :: rest))
        || all_diffs_between (with_prev (next :: rest)) low high
        || all_diffs_between (with_prev (curr :: rest)) low high
    | _ -> assert false
  in
  loop None xs

let are_safe_relaxed levels =
  let ignore_first_or_second =
    match levels with
    | f :: s :: rest -> are_safe (f :: rest) || are_safe (s :: rest)
    | _ -> assert false
  in
  ignore_first_or_second
  || get_diff_range levels
     |> Option.map ~f:(fun (l, h) -> all_diffs_between_relaxed levels l h)
     |> Option.value ~default:false

let solve filename =
  let input = read_file filename in
  let part1 = List.count input ~f:are_safe
  and part2 = List.count input ~f:are_safe_relaxed in
  (Some (Int.to_string part1), Some (Int.to_string part2))
