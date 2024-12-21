open Core
open Utils

let parse input =
  let regex = Re.compile (Re.Pcre.re "-?\\d+") in
  input |> String.split_lines
  |> List.map ~f:(fun line ->
         match Re.matches regex line |> List.map ~f:Int.of_string with
         | [ px; py; vx; vy ] -> ((px, py), (vx, vy))
         | _ -> assert false)

let calc_position ~width ~height ~initial:(px, py) ~velocity:(vx, vy) ~time =
  let x = (px + (vx * time)) % width in
  let y = (py + (vy * time)) % height in
  (x, y)

let part1 ~height ~width robots =
  let time = 100 in
  let positions =
    List.map robots ~f:(fun (initial, velocity) ->
        calc_position ~width ~height ~initial ~velocity ~time)
  in
  let mid_x = width / 2 and mid_y = height / 2 in
  let count_quad fx fy = List.count positions ~f:(fun (x, y) -> fx x && fy y) in
  let q1 = count_quad (Int.( < ) mid_x) (Int.( < ) mid_y) in
  let q2 = count_quad (Int.( > ) mid_x) (Int.( < ) mid_y) in
  let q3 = count_quad (Int.( > ) mid_x) (Int.( > ) mid_y) in
  let q4 = count_quad (Int.( < ) mid_x) (Int.( > ) mid_y) in
  q1 * q2 * q3 * q4

let at_time ~width ~height ~robots ~time =
  List.map robots ~f:(fun (initial, velocity) ->
      calc_position ~width ~height ~initial ~velocity ~time)
  |> List.map ~f:(fun (x, y) -> Point.create x y)
  |> Set.of_list (module Point)

let find_time ~height ~width robots ~p =
  Sequence.range 0 100000
  |> Sequence.find_map ~f:(fun time ->
         let positions = at_time ~height ~width ~robots ~time in
         if p positions then Some (time, positions) else None)

let has_horizontal_line ~length robots =
  Set.exists robots ~f:(fun p ->
      List.range 1 (length - 1)
      |> List.for_all ~f:(fun offset ->
             let p' = Point.move p (Point.create offset 0) in
             Set.mem robots p'))

let part2 ~height ~width robots =
  find_time ~height ~width robots ~p:(has_horizontal_line ~length:15)
  |> Option.value_exn |> fst

let solve filename =
  let height = 103 and width = 101 in
  let robots = filename |> Stdio.In_channel.read_all |> String.strip |> parse in
  let p1 = part1 ~height ~width robots in
  let p2 = part2 ~height ~width robots in
  (Some (Int.to_string p1), Some (Int.to_string p2))
