open Base
open Core
open Utils

type range = { xs : int * int; ys : int * int; zs : int * int }
[@@deriving sexp_of]

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

let range_to_str { xs = x1, x2; ys = y1, y2; zs = z1, z2 } =
  sprintf "x=%d..%d, y=%d..%d, z=%d..%d" x1 x2 y1 y2 z1 z2

let all_points range =
  let open Sequence.Let_syntax in
  let seq (p1, p2) = Sequence.range ~stop:`inclusive p1 p2 in
  let%bind x = seq range.xs in
  let%bind y = seq range.ys in
  let%map z = seq range.zs in
  (x, y, z)

let within_bounds input =
  let in_range (p1, p2) = -50 <= p1 && p1 <= 50 && -50 <= p2 && p2 <= 50 in
  List.filter input ~f:(fun (_, range) ->
      in_range range.xs && in_range range.ys && in_range range.ys)

let part1 () =
  let input = read_input "./input/day22.txt" |> within_bounds in
  let on = Hash_set.create (module Vec3) in
  let turn_on p = Hash_set.add on p in
  let turn_off p = Hash_set.remove on p in
  input
  |> List.iter ~f:(fun (state, range) ->
         let f = match state with `on -> turn_on | `off -> turn_off in
         all_points range |> Sequence.iter ~f);
  Hash_set.length on

let split_one_dim (p1, p2) =
  if p1 = p2 then [ (p1, p2) ]
  else if p2 - p1 = 1 then [ (p1, p1); (p2, p2) ]
  else
    let mid = (p1 + p2) / 2 in
    [ (p1, mid); (mid + 1, p2) ]

let split_range range =
  let open List.Let_syntax in
  let%bind xs = split_one_dim range.xs in
  let%bind ys = split_one_dim range.ys in
  let%map zs = split_one_dim range.zs in
  { xs; ys; zs }

let bounding_range ranges =
  let minmax f =
    ranges
    |> List.bind ~f:(fun r ->
           let p1, p2 = f r in
           [ p1; p2 ])
    |> List_ex.minmax ~compare:Int.compare
    |> Option.value_exn
  in
  let xs = minmax (fun r -> r.xs) in
  let ys = minmax (fun r -> r.ys) in
  let zs = minmax (fun r -> r.zs) in
  { xs; ys; zs }

let overlap r1 r2 =
  let within (p11, p12) (p21, p22) = p11 <= p21 && p12 >= p22 in
  let partial (p11, p12) (p21, p22) = not (p12 < p21 || p11 > p22) in
  if within r1.xs r2.xs && within r1.ys r2.ys && within r1.zs r2.zs then `within
  else if partial r1.xs r2.xs && partial r1.ys r2.ys && partial r1.zs r2.zs then
    `partial
  else `no_overlap

let find_containing_step range steps =
  let rec loop range steps =
    match steps with
    | [] -> `no_overlap
    | (state, step_range) :: rest as current -> (
        match overlap step_range range with
        | `within ->
            (* printf "Area %s is within %s\n" (range_to_str range)
               (range_to_str step_range); *)
            `defined state
        | `partial ->
            (* printf "Area %s partially overlaps with %s\n" (range_to_str range)
               (range_to_str step_range); *)
            `partial current
        | `no_overlap -> loop range rest)
  in
  loop range steps

let volume { xs = x1, x2; ys = y1, y2; zs = z1, z2 } =
  (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)

let part2 filename =
  let input = read_input filename in
  let input_rev = List.rev input in
  (* let boundary = { xs = (-50, 50); ys = (-50, 50); zs = (-50, 50) } in *)
  let boundary = input |> List.map ~f:snd |> bounding_range in
  let rec loop steps range =
    match find_containing_step range steps with
    | `defined `off | `no_overlap -> 0
    | `defined `on -> volume range
    | `partial steps ->
        split_range range |> List.sum (module Int) ~f:(loop steps)
  in
  loop input_rev boundary

let solve filename =
  let part2 = part2 filename in
  (None, Some (Int.to_string part2))

let test =
  let r1 = (2, 10) in
  let r2 = (-11, 11) in
  let outer = { xs = r1; ys = r1; zs = r1 } in
  let inner = { xs = r2; ys = r2; zs = r2 } in
  overlap outer inner
