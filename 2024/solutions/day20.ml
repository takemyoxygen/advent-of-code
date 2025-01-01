open Core
open Utils

let read_input filename =
  let find_char grid char =
    Array.find_mapi_exn grid ~f:(fun y row ->
        Array.find_mapi row ~f:(fun x ch ->
            if Char.equal ch char then Some (Point.create x y) else None))
  in

  let grid =
    In_channel.read_lines filename
    |> List.filter ~f:(fun line -> not (String.is_empty line))
    |> List.map ~f:String.to_array
    |> Array.of_list
  in

  (grid, find_char grid 'S', find_char grid 'E')

module Cheat = struct
  module T = struct
    type t = { start : Point.t; end_ : Point.t; cost : int }
    [@@deriving compare, fields ~getters ~iterators:create, sexp]

    let create = Fields.create
  end

  include T
  include Comparable.Make (T)
end

let directions =
  [
    Point.Direction.up;
    Point.Direction.down;
    Point.Direction.left;
    Point.Direction.right;
  ]

let can_move_into grid pos =
  match Point.element_at grid pos with
  | Some '#' | None -> false
  | Some ('.' | 'E' | 'S') -> true
  | Some char -> failwith [%string "Unexpected character: %{char#Char}"]

let adjacent_basic grid ~pos ~dist:_ =
  directions
  |> List.map ~f:(Point.move pos)
  |> List.filter ~f:(can_move_into grid)

let bfs ~start ~adjacent =
  let distances = Hashtbl.create (module Point) in
  let queue = Queue.create () in
  let () = Queue.enqueue queue (start, 0) in
  let rec loop () =
    if Queue.is_empty queue then distances
    else
      let pos, dist = Queue.dequeue_exn queue in
      if Hashtbl.mem distances pos then loop ()
      else
        let _ = Hashtbl.add distances ~key:pos ~data:dist in
        let () =
          adjacent ~pos ~dist
          |> List.filter ~f:(fun adj -> not (Hashtbl.mem distances adj))
          |> List.iter ~f:(fun p -> Queue.enqueue queue (p, dist + 1))
        in
        loop ()
  in
  loop ()

let adjacent_cheats grid ~max_dist ~pos ~dist =
  if dist >= max_dist then []
  else
    directions
    |> List.map ~f:(Point.move pos)
    |> List.filter ~f:(fun p -> Point.element_at grid p |> Option.is_some)

let find_all_cheats grid ~start ~cheat_destinations ~max_dist =
  bfs ~start ~adjacent:(adjacent_cheats grid ~max_dist)
  |> Hashtbl.to_alist
  |> List.filter ~f:(fun (end_, cost) ->
         Set.mem cheat_destinations end_ && cost > 0)
  |> List.map ~f:(fun (end_, cost) -> Cheat.create ~start ~end_ ~cost)

let solve filename =
  let grid, start, dest = read_input filename in
  let from_start = bfs ~start ~adjacent:(adjacent_basic grid) in
  let from_dest = bfs ~start:dest ~adjacent:(adjacent_basic grid) in
  let cheat_destinations =
    Hashtbl.keys from_dest |> Set.of_list (module Point)
  in
  let base_cost = Hashtbl.find_exn from_start dest in
  let find_cheats max_cheat =
    from_start |> Hashtbl.keys
    |> List.concat_map ~f:(fun start ->
           find_all_cheats grid ~start ~cheat_destinations ~max_dist:max_cheat)
    |> List.stable_dedup ~compare:Cheat.compare
    |> List.filter_map ~f:(fun cheat ->
           let before_cheat = Hashtbl.find from_start (Cheat.start cheat)
           and after_cheat = Hashtbl.find from_dest (Cheat.end_ cheat) in
           match (before_cheat, after_cheat) with
           | Some before, Some after -> Some (before + after + Cheat.cost cheat)
           | _ -> None)
    |> List.filter_map ~f:(fun cost ->
           if cost < base_cost then Some (base_cost - cost) else None)
    |> List.count ~f:(fun cost -> cost >= 100)
  in
  let p1 = find_cheats 2 and p2 = find_cheats 20 in
  (Some (Int.to_string p1), Some (Int.to_string p2))
