open Core
open Utils

let char_to_dir = function
  | '>' -> Point.Direction.right
  | '<' -> Point.Direction.left
  | '^' -> Point.Direction.up
  | 'v' -> Point.Direction.down
  | _ -> assert false

let read_input filename =
  match
    Stdio.In_channel.read_all filename
    |> String.strip
    |> Str.split (Str.regexp "\n\n")
  with
  | [ grid; moves ] ->
      let moves =
        moves
        |> String.substr_replace_all ~pattern:"\n" ~with_:""
        |> String.to_list |> List.map ~f:char_to_dir
      in
      let grid_lines = String.split_lines grid in
      let bottom_right =
        Point.create
          (String.length (List.hd_exn grid_lines) - 1)
          (List.length grid_lines - 1)
      in
      let walls, boxes, robot =
        grid_lines
        |> List.foldi
             ~init:(Set.empty (module Point), Set.empty (module Point), None)
             ~f:(fun y acc line ->
               line
               |> String.foldi ~init:acc
                    ~f:(fun x ((walls, boxes, robot) as acc) ch ->
                      match ch with
                      | '#' -> (Set.add walls (Point.create x y), boxes, robot)
                      | 'O' -> (walls, Set.add boxes (Point.create x y), robot)
                      | '@' -> (walls, boxes, Some (Point.create x y))
                      | _ -> acc))
      in
      (walls, boxes, Option.value_exn robot, moves, bottom_right)
  | _ -> assert false

let find_box ~box_width boxes pos =
  Sequence.range 0 box_width
  |> Sequence.find_map ~f:(fun n ->
         let box_start = Point.move_n pos ~n ~direction:Point.Direction.left in
         if Set.mem boxes box_start then Some box_start else None)

let classify ~box_width pos walls boxes =
  if Set.mem walls pos then `Wall
  else
    match find_box ~box_width boxes pos with
    | Some box_pos -> `Box box_pos
    | None -> `Empty

let rec find_movable_boxes ~box_width direction boxes walls pos =
  match classify ~box_width pos walls boxes with
  | `Empty -> Some (Set.empty (module Point))
  | `Wall -> None
  | `Box box_pos ->
      let next =
        match direction with
        | d when Point.(d = Direction.left) ->
            [ Point.move box_pos Point.Direction.left ]
        | d when Point.(d = Direction.right) ->
            [
              Point.move_n ~n:box_width box_pos ~direction:Point.Direction.right;
            ]
        | d ->
            List.range 0 box_width
            |> List.map ~f:(fun n ->
                   Point.move_n ~n box_pos ~direction:Point.Direction.right)
            |> List.map ~f:(Point.move d)
      in
      List.map next ~f:(find_movable_boxes ~box_width direction boxes walls)
      |> Option.all
      |> Option.map ~f:(Set.union_list (module Point))
      |> Option.map ~f:(fun s -> Set.add s box_pos)

let try_move ~box_width pos dir walls boxes =
  let next = Point.move pos dir in
  match classify ~box_width next walls boxes with
  | `Wall -> (pos, boxes)
  | `Empty -> (next, boxes)
  | `Box box_pos -> (
      match find_movable_boxes ~box_width dir boxes walls box_pos with
      | None -> (pos, boxes)
      | Some boxes_to_move ->
          let moved =
            Set.map (module Point) boxes_to_move ~f:(Point.move dir)
          in
          let boxes' = Set.diff boxes boxes_to_move |> Set.union moved in
          (next, boxes'))

let extend walls boxes robot =
  let walls' =
    walls |> Set.to_list
    |> List.concat_map ~f:(fun { Point.x; y } ->
           [ Point.create (x * 2) y; Point.create ((x * 2) + 1) y ])
    |> Set.of_list (module Point)
  in
  let boxes' =
    Set.map
      (module Point)
      boxes
      ~f:(fun { Point.x; y } -> Point.create (x * 2) y)
  in
  let robot' = Point.create (Point.x robot * 2) (Point.y robot) in
  (walls', boxes', robot')

let do_moves ~box_width walls boxes robot moves =
  moves
  |> List.fold ~init:(robot, boxes) ~f:(fun (robot, boxes) dir ->
         try_move ~box_width robot dir walls boxes)
  |> snd
  |> Set.fold ~init:0 ~f:(fun acc box ->
         acc + (Point.x box + (100 * Point.y box)))

let solve filename =
  let walls, boxes, robot, moves, _ = read_input filename in
  let p1 = do_moves ~box_width:1 walls boxes robot moves in
  let p2 =
    let walls, boxes, robot = extend walls boxes robot in
    do_moves ~box_width:2 walls boxes robot moves
  in
  (Some (Int.to_string p1), Some (Int.to_string p2))
