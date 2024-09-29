open Base
open Core
open Utils

type range = { xs : int * int; ys : int * int; zs : int * int }

let find_all str regex =
  let[@alert "-deprecated"] rec loop pos acc =
    if pos >= String.length str then List.rev acc
    else
      try
        let _ = Str.search_forward regex str pos in
        let m = Str.matched_string str in
        let pos' = Str.match_end () in
        loop pos' (m :: acc)
      with Not_found -> List.rev acc [@alert "+deprecated"]
  in

  loop 0 []

let coords_regex = Str.regexp "-?[0-9]+"

let parse_line line =
  match String.split line ~on:' ' with
  | [ state_s; coords_s ] -> (
      let state =
        match state_s with "on" -> `on | "off" -> `off | _ -> assert false
      in
      match find_all coords_s coords_regex |> List.map ~f:Int.of_string with
      | [ x1; x2; y1; y2; z1; z2 ] ->
          let range = { xs = (x1, x2); ys = (y1, y2); zs = (z1, z2) } in
          (state, range)
      | _ -> assert false)
  | _ -> assert false

let read_input filename =
  Stdio.In_channel.read_lines filename |> List.map ~f:parse_line

let all_points range =
  let open Sequence.Let_syntax in
  let seq (p1, p2) = Sequence.range ~stop:`inclusive p1 p2 in
  let%bind x = seq range.xs in
  let%bind y = seq range.ys in
  let%map z = seq range.zs in
  (x, y, z)

let test =
  let input = read_input "./input/day22.txt" in
  let on = Hash_set.create (module Vec3) in
  let turn_on p = Hash_set.add on p in
  let turn_off p = Hash_set.remove on p in
  let in_range (p1, p2) = -50 <= p1 && p1 <= 50 && -50 <= p2 && p2 <= 50 in
  input
  |> List.filter ~f:(fun (_, range) ->
         in_range range.xs && in_range range.ys && in_range range.ys)
  |> List.iter ~f:(fun (state, range) ->
         let f = match state with `on -> turn_on | `off -> turn_off in
         all_points range |> Sequence.iter ~f);
  Hash_set.length on
