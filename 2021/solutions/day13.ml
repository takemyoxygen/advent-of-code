open! Base
open! Core
open! Utils

let initial_points, folds =
  match
    Stdio.In_channel.read_all "./input/day13.txt"
    |> Str.split (Str.regexp "\n\n")
  with
  | [ coords_text; folds_text ] ->
      let coords =
        String.split_lines coords_text
        |> List.map ~f:(fun line ->
               match String.split ~on:',' line with
               | [ x; y ] -> Point.create (int_of_string x) (int_of_string y)
               | _ -> assert false)
      in
      let folds =
        String.split_lines folds_text
        |> List.map ~f:(fun line ->
               let along = if String.contains line 'y' then `y else `x in
               let value =
                 String.split line ~on:'=' |> List.last_exn |> int_of_string
               in
               (along, value))
      in
      (Set.of_list (module Point) coords, folds)
  | _ -> assert false

let fold points ~coord ~value ~new_point =
  let fold_point p =
    let delta = coord p - value in
    let value' = value - delta in
    new_point p value'
  in
  Set.map
    (module Point)
    points
    ~f:(fun p -> if coord p > value then fold_point p else p)

let fold_along_y y points =
  fold points ~coord:Point.y ~value:y ~new_point:(fun p y ->
      Point.create (Point.x p) y)

let fold_along_x x points =
  fold points ~coord:Point.x ~value:x ~new_point:(fun p x ->
      Point.create x (Point.y p))

let apply_fold points fold =
  match fold with
  | `x, x -> fold_along_x x points
  | `y, y -> fold_along_y y points

let minmax items =
  let min = List.min_elt ~compare:Int.compare items |> Option.value_exn in
  let max = List.max_elt ~compare:Int.compare items |> Option.value_exn in
  (min, max)

let part1 () = apply_fold initial_points (List.hd_exn folds) |> Set.length

let part2 () =
  let points =
    List.fold ~init:initial_points ~f:apply_fold folds |> Set.to_list
  in
  let min_x, max_x = points |> List.map ~f:Point.x |> minmax in
  let min_y, max_y = points |> List.map ~f:Point.y |> minmax in
  let grid =
    Array.make_matrix ~dimx:(max_y - min_y + 1) ~dimy:(max_x - min_x + 1) ' '
  in
  let () =
    List.iter points ~f:(fun p ->
        grid.(Point.y p - min_y).(Point.x p - min_x) <- '#')
  in
  let grid_text =
    grid |> Array.map ~f:String.of_array |> String.concat_array ~sep:"\n"
  in
  let () = printf "%s\n" grid_text in
  ()
