open! Core
open Utils

let parse_input filename =
  let lines = Stdio.In_channel.read_lines filename in
  let height = List.length lines in
  let width = String.length (List.hd_exn lines) in
  let scan y acc line =
    String.foldi line ~init:acc ~f:(fun x ((rights, downs) as acc) char ->
        match char with
        | '>' -> (Set.add rights (Point.create x y), downs)
        | 'v' -> (rights, Set.add downs (Point.create x y))
        | _ -> acc)
  in
  let init = Set.empty (module Point) in
  let rights, downs = List.foldi lines ~init:(init, init) ~f:scan in
  (rights, downs, height, width)

let next_pos pos dir height width =
  let next = Point.move pos dir in
  let coerce value limit = if value >= limit then 0 else value in
  Point.create (coerce (Point.x next) width) (coerce (Point.y next) height)

let move rights downs height width =
  let moved = ref false in
  let move_in_dir m herd dir another_herd =
    Set.map m herd ~f:(fun pos ->
        let next = next_pos pos dir height width in
        if Set.mem herd next || Set.mem another_herd next then pos
        else
          let () = moved := true in
          next)
  in
  let rights' = move_in_dir (module Point) rights Point.Direction.right downs in
  let downs' = move_in_dir (module Point) downs Point.Direction.down rights' in
  (rights', downs', !moved)

let move_until_stopped rights downs height width =
  let rec loop rights downs steps =
    let rights', downs', moved = move rights downs height width in
    if moved then loop rights' downs' (steps + 1) else steps + 1
  in
  loop rights downs 0

let test () =
  let rights, downs, height, width = parse_input "./input/day25-test.txt" in
  move_until_stopped rights downs height width

let solve filename =
  let rights, downs, height, width = parse_input filename in
  let part1 = move_until_stopped rights downs height width in
  (Some (Int.to_string part1), None)
