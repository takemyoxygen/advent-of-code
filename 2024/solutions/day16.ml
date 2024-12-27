open Core
open Utils

module Graph_node = struct
  type t = { position : Point.t; direction : Point.t }
  [@@deriving compare, fields ~getters, sexp_of, hash]

  let create ~position ~direction = { position; direction }
end

module Path_item = struct
  type t = { cost : int; node : Graph_node.t; path : Point.t list }
  [@@deriving compare, fields ~getters]

  let create ~cost ~position ~direction ~path =
    { cost; node = Graph_node.create ~position ~direction; path }

  let position t = Graph_node.position t.node
  let direction t = Graph_node.direction t.node
end

module Heap = Binary_heap.Make (Path_item)

let directons =
  [|
    Point.Direction.up;
    Point.Direction.right;
    Point.Direction.down;
    Point.Direction.left;
  |]

let rotated direction =
  let idx =
    Array_ex.index_of directons direction ~equal:Point.equal |> Option.value_exn
  in
  let clockwise = Array.get directons ((idx + 1) % Array.length directons) in
  let counter_clockwise =
    Array.get directons ((idx - 1) % Array.length directons)
  in
  (clockwise, counter_clockwise)

let next_moves grid current =
  let clock, counter = current |> Path_item.direction |> rotated in
  let dirs =
    [ (Path_item.direction current, 0); (clock, 1000); (counter, 1000) ]
  in
  let can_enter pos =
    match Point.element_at grid pos with None | Some '#' -> false | _ -> true
  in
  List.filter_map dirs ~f:(fun (dir, cost) ->
      let next_pos = Point.move (Path_item.position current) dir in
      if can_enter next_pos then
        Some
          (Path_item.create
             ~cost:(Path_item.cost current + cost + 1)
             ~position:next_pos ~direction:dir
             ~path:(next_pos :: Path_item.path current))
      else None)

let dijkstra grid start dest =
  let distances = Hashtbl.create (module Graph_node) in
  let heap =
    Heap.create
      ~dummy:
        (Path_item.create ~cost:0 ~position:Point.zero ~direction:Point.zero
           ~path:[])
      50
  in
  let rec loop shortest_paths =
    match Heap.pop_minimum heap with
    | item when Point.equal (Path_item.position item) dest -> (
        match shortest_paths with
        | [] -> loop [ item ]
        | shortest :: _ when Path_item.cost item > Path_item.cost shortest ->
            shortest_paths
        | _ -> loop (item :: shortest_paths))
    | item ->
        let adjacent = next_moves grid item in
        adjacent
        |> List.iter ~f:(fun item ->
               let node = Path_item.node item in
               let known_dist =
                 Hashtbl.find distances node
                 |> Option.value ~default:Int.max_value
               in
               if Path_item.cost item <= known_dist then
                 let () =
                   Hashtbl.set distances ~key:node ~data:(Path_item.cost item)
                 in
                 Heap.add heap item);
        loop shortest_paths
  in
  Heap.add heap
    (Path_item.create ~cost:0 ~position:start ~direction:Point.Direction.right
       ~path:[ start ]);
  loop []

let read_input filename =
  let grid =
    filename |> Stdio.In_channel.read_lines
    |> List.filter ~f:(fun line -> not (String.is_empty line))
    |> List.map ~f:String.to_array
    |> Array.of_list
  in
  let find char =
    Array.find_mapi grid ~f:(fun y row ->
        Array.find_mapi row ~f:(fun x c ->
            if Char.equal c char then Some (Point.create x y) else None))
    |> Option.value_exn
  in
  let start = find 'S' and dest = find 'E' in
  (grid, start, dest)

let solve filename =
  let grid, start, dest = read_input filename in
  let paths = dijkstra grid start dest in
  let p1 = List.hd_exn paths |> Path_item.cost in
  let p2 =
    paths
    |> List.concat_map ~f:Path_item.path
    |> List.dedup_and_sort ~compare:Point.compare
    |> List.length
  in
  (Some (Int.to_string p1), Some (Int.to_string p2))
