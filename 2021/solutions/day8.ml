open Base
open Core

let read_input filename =
  Stdio.In_channel.read_lines filename
  |> List.map ~f:(fun line ->
         let parts =
           Str.split (Str.regexp " | ") line
           |> List.map ~f:(String.split ~on:' ')
         in
         match parts with
         | [ signal; output ] -> (signal, output)
         | _ -> assert false)

let to_chars_set s = String.to_list s |> Set.of_list (module Char)

let find_only (xs : 'a list) ~f =
  match List.filter xs ~f with
  | [ s ] -> s
  | xs ->
      failwith
        (Printf.sprintf "Expected to have 1 result but %d found"
           (List.length xs))

let all_of_size size sets = List.filter sets ~f:(fun s -> Set.length s = size)

let find_by_size sets size =
  all_of_size size sets |> find_only ~f:(Fn.const true)

let not_any_of candidates s = List.exists candidates ~f:(Set.equal s) |> not

let deduct numbers =
  let num_sets = List.map numbers ~f:to_chars_set in
  let one = find_by_size num_sets 2 in
  let seven = find_by_size num_sets 3 in
  let four = find_by_size num_sets 4 in
  let eight = find_by_size num_sets 7 in
  let six =
    num_sets |> all_of_size 6
    |> find_only ~f:(fun s -> Set.inter s one |> Set.length = 1)
  in
  let five =
    num_sets |> all_of_size 5
    |> find_only ~f:(fun s -> Set.equal (Set.inter s six) s)
  in
  let nine =
    num_sets |> all_of_size 6
    |> find_only ~f:(fun s -> Set.equal (Set.inter s four) four)
  in
  let zero =
    num_sets |> all_of_size 6 |> find_only ~f:(not_any_of [ nine; six ])
  in
  let three =
    num_sets |> all_of_size 5
    |> find_only ~f:(fun s -> Set.equal (Set.inter s one) one)
  in
  let two =
    num_sets |> all_of_size 5 |> find_only ~f:(not_any_of [ five; three ])
  in
  [ zero; one; two; three; four; five; six; seven; eight; nine ]

let output_from (signals, outputs) =
  let digits = deduct signals in
  let outputs_sets = List.map outputs ~f:to_chars_set in
  let indices =
    outputs_sets
    |> List.map ~f:(fun d ->
           List.findi_exn digits ~f:(fun _ v -> Set.equal d v) |> Tuple2.get1)
  in
  List.fold ~init:0 ~f:(fun acc d -> (acc * 10) + d) indices

let solve filename =
  let input = read_input filename in
  let part1 =
    input
    |> List.concat_map ~f:(fun (_, output) -> output)
    |> List.filter ~f:(fun v ->
           let len = String.length v in
           len = 2 || len = 7 || len = 4 || len = 3)
    |> List.length
  in
  let part2 =
    input |> List.map ~f:output_from |> List.reduce ~f:( + ) |> Option.value_exn
  in
  (Some (Int.to_string part1), Some (Int.to_string part2))
