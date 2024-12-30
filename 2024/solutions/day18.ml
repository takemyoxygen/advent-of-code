open Core
open Utils

let read_input file =
  file |> In_channel.read_lines
  |> List.filter ~f:(fun line -> not (String.is_empty line))
  |> List.map ~f:(fun line ->
         match String.split line ~on:',' with
         | [ x; y ] -> Point.create (Int.of_string x) (Int.of_string y)
         | _ -> failwith "Invalid input")

let bfs ~start ~adjacent ~dest =
  let visited = Hash_set.create (module Point) in
  let queue = Queue.create () in
  let () = Queue.enqueue queue (start, 0) in
  let rec loop () =
    if Queue.is_empty queue then `No_path
    else
      let p, dist = Queue.dequeue_exn queue in
      if Point.equal p dest then `Path dist
      else if Hash_set.mem visited p then loop ()
      else
        let () = Hash_set.add visited p in
        let () =
          adjacent p visited
          |> List.filter ~f:(fun adj -> not (Hash_set.mem visited adj))
          |> List.iter ~f:(fun p -> Queue.enqueue queue (p, dist + 1))
        in
        loop ()
  in
  loop ()

let within_bounds { Point.x; y } ~top_right:{ Point.x = tr_x; y = tr_y } =
  x >= 0 && y >= 0 && x <= tr_x && y <= tr_y

let directions =
  [
    Point.Direction.up;
    Point.Direction.down;
    Point.Direction.left;
    Point.Direction.right;
  ]

let part1 points top_right =
  let fallen = points |> Set.of_list (module Point) in
  match
    bfs ~start:Point.zero ~dest:top_right ~adjacent:(fun p _ ->
        directions
        |> List.map ~f:(fun dir -> Point.move p dir)
        |> List.filter ~f:(fun p ->
               within_bounds p ~top_right && not (Set.mem fallen p)))
  with
  | `Path dist -> dist
  | `No_path -> failwith "No path found"

let part2 points top_right =
  let check_path fallen_bytes =
    let fallen = List.take points fallen_bytes |> Set.of_list (module Point) in
    match
      bfs ~start:Point.zero ~dest:top_right ~adjacent:(fun p _ ->
          directions
          |> List.map ~f:(fun dir -> Point.move p dir)
          |> List.filter ~f:(fun p ->
                 within_bounds p ~top_right && not (Set.mem fallen p)))
    with
    | `Path _ -> true
    | `No_path -> false
  in
  let rec find_byte lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if check_path mid then find_byte (mid + 1) hi else find_byte lo mid
  in
  let num_fallen = find_byte 0 (List.length points) in
  let { Point.x; y } = List.take points num_fallen |> List.last_exn in
  sprintf "%d,%d" x y

let solve filename =
  let points = read_input filename in
  let top_right = Point.create 70 70 in
  let p1 = part1 (List.take points 1024) top_right in
  let p2 = part2 points top_right in
  (Some (Int.to_string p1), Some p2)
