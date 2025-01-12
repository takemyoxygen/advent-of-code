open Core

let read_input filename =
  filename |> In_channel.read_lines
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:(fun line ->
         match String.split line ~on:'-' with
         | [ a; b ] -> (a, b)
         | _ -> assert false)

let to_map edges =
  let add_edge from to_ map =
    Map.update map from ~f:(function
      | None -> Set.singleton (module String) to_
      | Some s -> Set.add s to_)
  in
  List.fold edges
    ~init:(Map.empty (module String))
    ~f:(fun map (a, b) -> map |> add_edge a b |> add_edge b a)

let find_connected_grops ~size adj_map =
  let rec loop acc possible =
    if List.length acc = size then [ acc ]
    else if Set.length possible = 0 then []
    else
      Set.to_list possible
      |> List.concat_map ~f:(fun p ->
             loop (p :: acc)
               (Set.inter possible
                  (Map.find adj_map p
                  |> Option.value ~default:(Set.empty (module String))
                  |> Set.filter ~f:(fun adj -> String.compare p adj > 0))))
  in
  loop [] (Map.key_set adj_map)

let find_largest_connected adj_map =
  let rec loop acc possible =
    if Set.length possible = 0 then acc
    else
      Set.to_list possible
      |> List.map ~f:(fun p ->
             loop (p :: acc)
               (Set.inter possible
                  (Map.find adj_map p
                  |> Option.value ~default:(Set.empty (module String))
                  |> Set.filter ~f:(fun adj -> String.compare p adj > 0))))
      |> List.max_elt ~compare:(Comparable.lift Int.compare ~f:List.length)
      |> Option.value ~default:[]
  in
  loop [] (Map.key_set adj_map)

let part1 adj_map =
  adj_map
  |> find_connected_grops ~size:3
  |> List.count ~f:(List.exists ~f:(String.is_prefix ~prefix:"t"))

let part2 adj_map =
  adj_map |> find_largest_connected
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:","

let solve filename =
  let adj_map = read_input filename |> to_map in
  let p1 = part1 adj_map in
  let p2 = part2 adj_map in
  (Some (Int.to_string p1), Some p2)
