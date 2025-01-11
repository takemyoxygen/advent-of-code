open Core
open Utils

let directions =
  [
    (Point.Direction.up, '^');
    (Point.Direction.right, '>');
    (Point.Direction.down, 'v');
    (Point.Direction.left, '<');
  ]

let numeric_keypad =
  [|
    [| '7'; '8'; '9' |];
    [| '4'; '5'; '6' |];
    [| '1'; '2'; '3' |];
    [| 'X'; '0'; 'A' |];
  |]

let directional_keypad = [| [| 'X'; '^'; 'A' |]; [| '<'; 'v'; '>' |] |]

let flip_key = function
  | '<' -> '>'
  | '>' -> '<'
  | 'v' -> '^'
  | '^' -> 'v'
  | _ -> assert false

let find_button_pos keypad button =
  Array.find_mapi_exn keypad ~f:(fun y row ->
      Array_ex.index_of row button ~equal:Char.equal
      |> Option.map ~f:(fun x -> Point.create x y))

(* all shortest paths between two points *)
let bfs ~start ~dest ~adjacent =
  let distances = Hashtbl.create (module Point) in
  let queue = Queue.create () in
  let () = Queue.enqueue queue (start, 0) in
  let rec loop () : unit =
    match Queue.dequeue queue with
    | None -> failwith "No paths found"
    | Some (pos, _) when Hashtbl.mem distances pos -> loop ()
    | Some (pos, dist) ->
        Hashtbl.set distances ~key:pos ~data:dist;
        if Point.equal pos dest then ()
        else
          let () =
            adjacent ~pos
            |> List.filter ~f:(fun (adj, _) -> not (Hashtbl.mem distances adj))
            |> List.iter ~f:(fun (adj, _) ->
                   Queue.enqueue queue (adj, dist + 1))
          in
          loop ()
  in
  let rec traverse_back paths dist =
    if dist = 0 then paths
    else
      let paths' =
        List.concat_map paths ~f:(fun (keys, pos) ->
            adjacent ~pos
            |> List.filter ~f:(fun (adj, _) ->
                   match Hashtbl.find distances adj with
                   | Some d when d = dist - 1 -> true
                   | _ -> false)
            |> List.map ~f:(fun (adj, key) -> (flip_key key :: keys, adj)))
      in
      traverse_back paths' (dist - 1)
  in
  loop ();
  let dist = Hashtbl.find_exn distances dest in
  traverse_back [ ([], dest) ] dist |> List.map ~f:fst

let adjacent keypad ~pos =
  directions
  |> List.map ~f:(fun (dir, key) -> (Point.move pos dir, key))
  |> List.filter ~f:(fun (adj, _) ->
         match Point.element_at keypad adj with
         | None | Some 'X' -> false
         | Some _ -> true)

let press keypad current target =
  let start = find_button_pos keypad current in
  let dest = find_button_pos keypad target in
  bfs ~start ~dest ~adjacent:(adjacent keypad)

module Key_pair = struct
  type t = char * char [@@deriving compare, equal, hash, sexp]
end

let build_paths_cache keypad =
  let all_pairs =
    keypad |> List.of_array
    |> List.concat_map ~f:List.of_array
    |> List.filter ~f:(Fn.non (Char.equal 'X'))
    |> fun l -> List.cartesian_product l l
  in
  let paths =
    all_pairs
    |> List.map ~f:(fun (a, b) -> ((a, b), press keypad a b))
    |> Hashtbl.of_alist_exn (module Key_pair)
  in
  paths

(* what keys to press on the directional keypad to get [keys] 
to be pressed on the underlying keypad *)
let press_sequence paths keys =
  List.fold keys ~init:([ [] ], 'A') ~f:(fun (dirs, prev_key) key ->
      let new_paths = Hashtbl.find_exn paths (prev_key, key) in
      let dirs' =
        let%bind.List prev = dirs in
        let%map.List new_ = new_paths in
        prev @ new_ @ [ 'A' ]
      in
      (dirs', key))
  |> fst

let calculate_directional_cost directional_paths num_of_keypads =
  let cache =
    Array.init num_of_keypads ~f:(fun _ -> Hashtbl.create (module Key_pair))
  in
  let rec traverse_path path left =
    match left with
    | 0 -> List.length path
    | left ->
        List.fold path ~init:('A', 0) ~f:(fun (prev, acc) curr ->
            (curr, acc + traverse_pair prev curr left))
        |> snd
  and traverse_pair from to_ left =
    match Hashtbl.find cache.(num_of_keypads - left) (from, to_) with
    | Some cost -> cost
    | None ->
        Hashtbl.find_or_add
          cache.(num_of_keypads - left)
          (from, to_)
          ~default:(fun () ->
            Hashtbl.find_exn directional_paths (from, to_)
            |> List.map ~f:(fun path ->
                   traverse_path (path @ [ 'A' ]) (left - 1))
            |> List.min_elt ~compare:Int.compare
            |> Option.value_exn)
  in
  fun path -> traverse_path path num_of_keypads

let calculate_cost ~calculate_directional_cost numeric_paths numeric_code =
  let directional_paths =
    press_sequence numeric_paths (String.to_list numeric_code)
  in
  List.map directional_paths ~f:calculate_directional_cost
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn

let find_total_complexity ?(directional_keypads = 2) directional_paths
    numeric_paths codes =
  let calculate_directional_cost =
    calculate_directional_cost directional_paths directional_keypads
  in
  List.map codes ~f:(fun code ->
      let num =
        code |> String.subo ~len:(String.length code - 1) |> Int.of_string
      in
      let length =
        calculate_cost numeric_paths ~calculate_directional_cost code
      in
      num * length)
  |> List.sum (module Int) ~f:Fn.id

let solve filename =
  let directional_paths = build_paths_cache directional_keypad in
  let numeric_paths = build_paths_cache numeric_keypad in
  let codes =
    In_channel.read_lines filename |> List.filter ~f:(Fn.non String.is_empty)
  in
  let go n =
    find_total_complexity ~directional_keypads:n directional_paths numeric_paths
      codes
  in
  let p1 = go 2 in
  let p2 = go 25 in
  (Some (Int.to_string p1), Some (Int.to_string p2))
