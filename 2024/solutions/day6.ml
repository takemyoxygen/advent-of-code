open Core
open Utils

let directions =
  [|
    Point.Direction.up;
    Point.Direction.right;
    Point.Direction.down;
    Point.Direction.left;
  |]

let turn_clockwise dir =
  let idx =
    Array_ex.index_of directions ~equal:Point.equal dir |> Option.value_exn
  in
  directions.((idx + 1) % Array.length directions)

let was_there_already visited pos dir =
  match Map.find visited pos with
  | Some dirs when Set.mem dirs dir -> true
  | _ -> false

let find_start grid =
  let start =
    Array.find_mapi grid ~f:(fun y row ->
        match Array_ex.index_of row '^' ~equal:Char.equal with
        | Some x -> Some (Point.create x y)
        | None -> None)
    |> Option.value_exn
  in
  grid.(Point.y start).(Point.x start) <- '.';
  start

let move grid start =
  let track_visit visited pos dir =
    Map.update visited pos ~f:(fun dirs ->
        Option.value dirs ~default:(Set.empty (module Point)) |> fun s ->
        Set.add s dir)
  in
  let rec loop visited pos dir =
    if was_there_already visited pos dir then `Loop
    else
      let visited = track_visit visited pos dir in
      let next = Point.move pos dir in
      match Point.element_at grid next with
      | None -> `Left_map visited
      | Some '.' -> loop visited next dir
      | Some '#' -> loop visited pos (turn_clockwise dir)
      | _ -> assert false
  in
  loop (Map.empty (module Point)) start Point.Direction.up

let put_obstacle grid { Point.x; y } =
  Array.mapi grid ~f:(fun ry row ->
      if ry = y then Array.mapi row ~f:(fun rx ch -> if x = rx then '#' else ch)
      else row)

let part1 grid start =
  match move grid start with
  | `Left_map visited -> Map.length visited
  | _ -> assert false

let part2 grid start =
  let visited =
    match move grid start with
    | `Left_map visited -> visited
    | _ -> assert false
  in
  visited |> Map.keys
  |> List.filter ~f:Point.(( <> ) start)
  |> List.filter_map ~f:(fun pos ->
         let grid' = put_obstacle grid pos in
         match move grid' start with `Left_map _ -> None | `Loop -> Some pos)
  |> List.length

let solve filename =
  let grid =
    filename |> Stdio.In_channel.read_lines
    |> List.map ~f:String.to_array
    |> List.to_array
  in
  let start = find_start grid in
  let p1 = part1 grid start and p2 = part2 grid start in
  (Some (Int.to_string p1), Some (Int.to_string p2))
