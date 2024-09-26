open Base
open Core
open Utils

module CharPair = struct
  type t = char * char

  include Tuple.Comparable (Char) (Char)
end

module MemoKey = struct
  type t = int * char * char [@@deriving sexp_of, sexp, hash, compare]
end

let read_input filename =
  let as_two_chars str =
    match String.to_list str with [ c1; c2 ] -> (c1, c2) | _ -> assert false
  in
  match Stdio.In_channel.read_lines filename with
  | template :: _ :: rules_lines ->
      let rules =
        List.map rules_lines ~f:(fun line ->
            match Str.split (Str.regexp " -> ") line with
            | [ combo; replacement ] ->
                ( as_two_chars combo,
                  replacement |> String.to_list |> List.hd_exn )
            | _ -> assert false)
        |> Map.of_alist_exn (module CharPair)
      in
      (template, rules)
  | _ -> assert false

let pairs_in xs =
  Sequence.unfold ~init:xs ~f:(function
    | fst :: (snd :: _ as next) -> Some ((fst, snd), next)
    | _ -> None)

let bump key counts =
  Map.update counts key ~f:(function None -> 1 | Some x -> x + 1)

let merge c1 c2 =
  Map.merge c1 c2 ~f:(fun ~key:_ el ->
      match el with
      | `Both (l, r) -> Some (l + r)
      | `Left x | `Right x -> Some x)

let make_find_counts replacement_rules =
  let memo = Hashtbl.create (module MemoKey) in
  let rec f_inner steps_left ((c1, c2) as pair) =
    match (steps_left, Map.find replacement_rules pair) with
    | 0, _ | _, None -> Map.empty (module Char)
    | _, Some r ->
        let left = f_memo (steps_left - 1) (c1, r) in
        let right = f_memo (steps_left - 1) (r, c2) in
        merge left right |> bump r
  and f_memo steps_left ((c1, c2) as pair) =
    Hashtbl.find_or_add memo (steps_left, c1, c2) ~default:(fun () ->
        f_inner steps_left pair)
  in
  f_memo

let solve_part template find_counts steps =
  let chars = String.to_list template in
  let pairs = pairs_in chars in
  let counts_inner =
    Sequence.map pairs ~f:(find_counts steps) |> Sequence.reduce_exn ~f:merge
  in
  let counts =
    List.fold chars ~init:counts_inner ~f:(fun acc ch -> bump ch acc)
  in
  let min, max =
    Map.to_alist counts
    |> List.map ~f:(fun (_, v) -> v)
    |> List_ex.minmax ~compare:Int.compare
    |> Option.value_exn
  in
  max - min

let solve filename =
  let template, replacement_rules = read_input filename in
  let find_count = make_find_counts replacement_rules in
  let part1 = solve_part template find_count 10 in
  let part2 = solve_part template find_count 40 in
  (Some (Int.to_string part1), Some (Int.to_string part2))
