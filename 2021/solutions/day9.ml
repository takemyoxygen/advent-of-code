open! Base
open Core
open Utils

let read_input filename =
  Stdio.In_channel.read_lines filename
  |> List.map ~f:(fun line ->
         String.to_list line |> List.map ~f:Char.get_digit_exn |> Array.of_list)
  |> Array.of_list

let low_points grid =
  Array.concat_mapi grid ~f:(fun row_idx row ->
      Array.filter_mapi row ~f:(fun col_idx x ->
          let above = row_idx = 0 || grid.(row_idx - 1).(col_idx) > x in
          let below =
            row_idx = Array.length grid - 1 || grid.(row_idx + 1).(col_idx) > x
          in
          let right =
            col_idx = Array.length row - 1 || grid.(row_idx).(col_idx + 1) > x
          in
          let left = col_idx = 0 || grid.(row_idx).(col_idx - 1) > x in
          if above && below && right && left then
            Some (Point.create col_idx row_idx, x)
          else None))

let neighbors grid point =
  [
    Point.Direction.up;
    Point.Direction.down;
    Point.Direction.left;
    Point.Direction.right;
  ]
  |> List.map ~f:(Point.move point)
  |> List.filter_map ~f:(fun pos ->
         match Point.element_at grid pos with
         | Some 9 | None -> None
         | Some _ -> Some pos)

let bfs grid start =
  let visited = Hash_set.create (module Point) in
  let queue = Queue.create () in
  let () = Queue.enqueue queue start in
  let rec loop () =
    if Queue.is_empty queue then visited |> Hash_set.to_list
    else
      let p = Queue.dequeue_exn queue in
      if Hash_set.mem visited p then loop ()
      else
        let () = Hash_set.add visited p in
        let () = neighbors grid p |> List.iter ~f:(Queue.enqueue queue) in
        loop ()
  in
  loop ()

let solve filename =
  let input = read_input filename in
  let part1 =
    let lows = low_points input in
    Array.fold lows ~init:0 ~f:(fun acc (_, x) -> acc + x) + Array.length lows
  in
  let part2 =
    low_points input |> Array.to_list
    |> List.map ~f:(fun (p, _) -> bfs input p |> List.length)
    |> List.sort ~compare:Int.descending
    |> (fun l -> List.take l 3)
    |> List.fold ~init:1 ~f:( * )
  in
  (Some (Int.to_string part1), Some (Int.to_string part2))
