open Base
open Utils

let input =
  Stdio.In_channel.read_lines "./input/day11.txt"
  |> List.map ~f:(fun line ->
         String.to_array line |> Array.map ~f:Char.get_digit_exn)
  |> Array.of_list

let offsets =
  let d = [ -1; 0; 1 ] in
  List.cartesian_product d d
  |> List.map ~f:(fun (x, y) -> Point.create x y)
  |> List.filter ~f:(Point.( <> ) Point.zero)

let neighbors grid pos =
  List.map offsets ~f:(Point.move pos)
  |> List.filter_map ~f:(fun neigh ->
         match Point.element_at grid neigh with
         | Some _ -> Some neigh
         | _ -> None)

let all_coords =
  let rows = List.range 0 (Array.length input) in
  let cols = List.range 0 (Array.length input.(0)) in
  List.cartesian_product rows cols
  |> List.map ~f:(fun (y, x) -> Point.create x y)

let step grid =
  let flashed = Hash_set.create (module Point) in
  let bump_queue = Queue.create () in
  let enqueue = List.iter ~f:(Queue.enqueue bump_queue) in
  let () = enqueue all_coords in
  let rec loop () =
    match Queue.dequeue bump_queue with
    | None -> ()
    | Some p when Hash_set.mem flashed p -> loop ()
    | Some p ->
        let value = Point.element_at grid p |> Option.value_exn in
        let () = grid.(Point.y p).(Point.x p) <- value + 1 in
        let () =
          if value = 9 then
            let () = Hash_set.add flashed p in
            neighbors grid p |> enqueue
        in
        loop ()
  in
  let () = loop () in
  let () =
    Hash_set.iter flashed ~f:(fun p -> grid.(Point.y p).(Point.x p) <- 0)
  in
  Hash_set.to_list flashed

let part1 () =
  let input = Array.copy_matrix input in
  List.range 0 100
  |> List.fold ~init:0 ~f:(fun acc _ ->
         let flashed = step input in
         acc + List.length flashed)

let part2 () =
  let input = Array.copy_matrix input in
  let size = List.length all_coords in
  let rec loop s =
    if step input |> List.length = size then s else loop (s + 1)
  in
  loop 1
