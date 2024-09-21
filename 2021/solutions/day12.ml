open! Base
open Core

let read_input filename =
  Stdio.In_channel.read_lines filename
  |> List.map ~f:(fun line ->
         match String.split ~on:'-' line with
         | [ c1; c2 ] -> (c1, c2)
         | _ -> assert false)

let make_graph input =
  List.append input (List.Assoc.inverse input)
  |> List.fold
       ~init:(Map.empty (module String))
       ~f:(fun acc (start, fin) ->
         Map.update acc start ~f:(fun ex -> fin :: Option.value ex ~default:[]))

let make_multi_visits graph =
  Map.keys graph
  |> List.filter ~f:(fun node ->
         String.to_list node |> List.for_all ~f:Char.is_uppercase)
  |> Set.of_list (module String)

let find_paths ~can_go ~graph ~multi_visits start dest =
  let rec loop node visited path_so_far =
    match node with
    | n when String.(n = dest) -> [ dest :: path_so_far ]
    | _ ->
        let adjacent = Map.find graph node |> Option.value ~default:[] in
        let visited' =
          if Set.mem multi_visits node then visited
          else
            Map.update visited node ~f:(function None -> 1 | Some x -> x + 1)
        in
        let next =
          adjacent
          |> List.filter ~f:(fun adj ->
                 Set.mem multi_visits adj || can_go visited' adj)
        in
        let path' = node :: path_so_far in
        next
        |> List.map ~f:(fun adj -> loop adj visited' path')
        |> List.fold ~init:[] ~f:List.append
  in
  let all_paths = loop start (Map.empty (module String)) [] in
  all_paths |> List.map ~f:List.rev
  |> List.dedup_and_sort ~compare:(List.compare String.compare)

let solve filename =
  let input = read_input filename in
  let graph = make_graph input in
  let multi_visits = make_multi_visits graph in
  let part1 =
    find_paths ~graph ~multi_visits "start" "end" ~can_go:(fun visited adj ->
        not (Map.mem visited adj))
    |> List.length
  in
  let part2 =
    find_paths ~graph ~multi_visits "start" "end" ~can_go:(fun visited adj ->
        match (adj, Map.find visited adj) with
        | ("start" | "end"), None -> true
        | ("start" | "end"), _ -> false
        | _, None -> true
        | _, Some 1 when Map.for_all visited ~f:(fun vis -> vis < 2) -> true
        | _ -> false)
    |> List.length
  in
  (Some (Int.to_string part1), Some (Int.to_string part2))
