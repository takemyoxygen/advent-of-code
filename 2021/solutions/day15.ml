open Base
open Core
open Utils

let input =
  Stdio.In_channel.read_lines "./input/day15.txt"
  |> List.map ~f:(fun line ->
         String.to_array line |> Array.map ~f:Char.get_digit_exn)
  |> Array.of_list

module Path = struct
  type t = int * Point.t [@@deriving compare]
end

module Heap = Binary_heap.Make (Path)

let directons =
  [
    Point.Direction.up;
    Point.Direction.down;
    Point.Direction.left;
    Point.Direction.right;
  ]

let dijkstra grid start dest =
  let distances = Hashtbl.create (module Point) in
  let heap = Heap.create ~dummy:(0, Point.zero) 10 in
  let visited = Hash_set.create (module Point) in
  let rec loop () =
    match Heap.pop_minimum heap with
    | _, pos when Hash_set.mem visited pos -> loop ()
    | path, pos when Point.equal pos dest -> path
    | path, pos ->
        let adjacent =
          directons
          |> List.map ~f:(Point.move pos)
          |> List.filter_map ~f:(fun pos ->
                 Point.element_at grid pos |> Option.map ~f:(fun x -> (pos, x)))
        in
        adjacent
        |> List.filter ~f:(fun (adj, _) -> not (Hash_set.mem visited adj))
        |> List.iter ~f:(fun (adj, x) ->
               let known_dist =
                 Hashtbl.find distances adj
                 |> Option.value ~default:Int.max_value
               in
               let new_value = min known_dist (path + x) in
               if new_value < known_dist then
                 Hashtbl.set distances ~key:adj ~data:new_value;
               Heap.add heap (new_value, adj));
        Hash_set.add visited pos;
        loop ()
  in
  Heap.add heap (0, start);
  loop ()

let extend xs =
  let incr xs step =
    Array.map xs ~f:(fun x ->
        match x + step with x' when x' >= 10 -> x' - 9 | x' -> x')
  in
  let repeat_right xs =
    List.range 0 5 |> List.map ~f:(incr xs) |> Array.concat
  in
  let extended_right = Array.map xs ~f:repeat_right in
  List.range 0 5
  |> List.map ~f:(fun step ->
         Array.map extended_right ~f:(fun row -> incr row step))
  |> Array.concat

let solve grid =
  let start = Point.zero in
  let dest = Point.create (Array.length grid.(0) - 1) (Array.length grid - 1) in
  dijkstra grid start dest

let part1 () = solve input
let part2 () = input |> extend |> solve
