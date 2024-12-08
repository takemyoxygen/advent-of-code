open Core
open Utils

let parse_input input =
  let grid =
    input |> String.split_lines |> List.map ~f:String.to_array |> Array.of_list
  in
  let antennas =
    Array.foldi grid
      ~init:(Map.empty (module Char))
      ~f:(fun y acc row ->
        Array.foldi row ~init:acc ~f:(fun x acc ch ->
            if Char.is_alphanum ch then
              Map.add_multi acc ~key:ch ~data:(Point.create x y)
            else acc))
  in
  (grid, antennas)

let antinodes_of_pair_1 p1 p2 =
  let dist = Point.sub p1 p2 in
  let anti1 = Point.move p1 dist in
  let anti2 = Point.sub p2 dist in
  [ anti1; anti2 ]

let antinodes_of_pair_2 grid p1 p2 =
  let unfold start dist =
    Sequence.unfold ~init:start ~f:(fun pos ->
        if Point.element_at grid pos |> Option.is_some then
          Some (pos, Point.move pos dist)
        else None)
  in
  let diff = Point.sub p1 p2 in
  Sequence.append (unfold p1 diff) (unfold p1 (Point.negate diff))
  |> Sequence.to_list

let antinodes_of antinodes_of_pair grid antennas =
  List_ex.all_pairs antennas
  |> List.concat_map ~f:(fun (p1, p2) -> antinodes_of_pair p1 p2)
  |> List.filter ~f:(fun pos -> Point.element_at grid pos |> Option.is_some)
  |> Set.of_list (module Point)

let part grid antennas antinodes_of_pair =
  Map.fold antennas
    ~init:(Set.empty (module Point))
    ~f:(fun ~key:_ ~data acc ->
      Set.union acc (antinodes_of antinodes_of_pair grid data))
  |> Set.length

let solve filename =
  let grid, antennas = filename |> Stdio.In_channel.read_all |> parse_input in
  let p1 = part grid antennas antinodes_of_pair_1
  and p2 = part grid antennas (antinodes_of_pair_2 grid) in
  (Some (Int.to_string p1), Some (Int.to_string p2))
