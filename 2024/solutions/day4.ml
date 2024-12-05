open Core
open Utils

let xmas = "XMAS"

let read_file filename =
  Stdio.In_channel.read_lines filename
  |> List.map ~f:String.to_array
  |> Array.of_list

let directions =
  Point.Direction.
    [ up; down; left; right; down_left; down_right; up_left; up_right ]

let dir_points start dir n =
  Sequence.unfold
    ~f:(fun (pos, n) ->
      match n with 0 -> None | n -> Some (pos, (Point.move pos dir, n - 1)))
    ~init:(start, n)
  |> Sequence.to_list

let part1 grid =
  let xmas_from pos =
    directions
    |> List.count ~f:(fun dir ->
           dir_points pos dir (String.length xmas)
           |> List.map ~f:(Point.element_at grid)
           |> List.map ~f:(Option.value ~default:' ')
           |> String.of_list |> String.equal xmas)
  in
  Array.concat_mapi grid ~f:(fun y line ->
      Array.mapi line ~f:(fun x ch ->
          if Char.equal ch 'X' then xmas_from (Point.create x y) else 0))
  |> Array.sum (module Int) ~f:Fn.id

let part2 grid =
  let diags =
    [
      [ Point.Direction.up_left; Point.Direction.down_right ];
      [ Point.Direction.up_right; Point.Direction.down_left ];
    ]
  in
  let ms = [ 'M'; 'S' ] |> Set.of_list (module Char) in
  let x_mas_at pos =
    diags
    |> List.for_all ~f:(fun diag ->
           diag
           |> List.map ~f:(Point.move pos)
           |> List.map ~f:(Point.element_at grid)
           |> List.filter_map ~f:Fn.id
           |> Set.of_list (module Char)
           |> Set.equal ms)
  in
  Array.mapi grid ~f:(fun y line ->
      Array.counti line ~f:(fun x ch ->
          Char.equal ch 'A' && x_mas_at (Point.create x y)))
  |> Array.sum (module Int) ~f:Fn.id

let solve filename =
  let input = read_file filename in
  let p1 = part1 input and p2 = part2 input in
  (Some (Int.to_string p1), Some (Int.to_string p2))
