open Base
open Core

type range = { xs : int * int; ys : int * int; zs : int * int }
[@@deriving sexp_of, equal]

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

let within_bounds input =
  let in_range (p1, p2) = -50 <= p1 && p1 <= 50 && -50 <= p2 && p2 <= 50 in
  List.filter input ~f:(fun (_, range) ->
      in_range range.xs && in_range range.ys && in_range range.ys)

let volume { xs = x1, x2; ys = y1, y2; zs = z1, z2 } =
  (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)

let overlap_1d (a1, a2) (b1, b2) = not (b1 > a2 || b2 < a1)

let split_range_1d ((a1, a2) as a) ((b1, b2) as b) =
  let in_a x = a1 <= x && x <= a2 in
  let ranges =
    if not (overlap_1d a b) then []
    else
      let nexts = List.sort [ a2; b1 - 1; b2 ] ~compare:Int.compare in
      let rec loop start nexts acc =
        if in_a start then
          match List.filter nexts ~f:(fun n -> n >= start) with
          | [] -> acc
          | end_ :: rest -> loop (end_ + 1) rest ((start, end_) :: acc)
        else acc
      in
      loop a1 nexts []
  in
  ranges
  |> List.filter ~f:(fun (x1, x2) -> x1 <= x2)
  |> List.sort ~compare:[%compare: int * int]

let overlaps_1d ranges range = List.filter ranges ~f:(overlap_1d range)

let split_dim a b =
  let splits = split_range_1d a b in
  let overlaps = overlaps_1d splits b in
  (splits, List.hd overlaps)

let subtract (cuboid1 : range) (cuboid2 : range) =
  let xs, split_x = split_dim cuboid1.xs cuboid2.xs
  and ys, split_y = split_dim cuboid1.ys cuboid2.ys
  and zs, split_z = split_dim cuboid1.zs cuboid2.zs in
  let to_deduct =
    let%map.Option sx = split_x and sy = split_y and sz = split_z in
    { xs = sx; ys = sy; zs = sz }
  in
  match to_deduct with
  | None -> [ cuboid1 ]
  | Some d ->
      let sub_ranges =
        let%map.List xs = xs and ys = ys and zs = zs in
        { xs; ys; zs }
      in
      List.filter sub_ranges ~f:(fun sr -> not (equal_range sr d))

let non_overlapping_ons instructions =
  let handle_instruction ons instruction =
    let ons' =
      List.concat_map ons ~f:(fun on -> subtract on (snd instruction))
    in
    match instruction with `on, range -> range :: ons' | `off, _ -> ons'
  in
  List.fold instructions ~init:[] ~f:handle_instruction

let solve filename =
  let solve_part instrs =
    non_overlapping_ons instrs |> List.sum (module Int) ~f:volume
  in
  let input = read_input filename in
  let part1 = solve_part (within_bounds input) in
  let part2 = solve_part input in
  (Some (Int.to_string part1), Some (Int.to_string part2))
