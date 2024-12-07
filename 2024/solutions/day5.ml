open! Core
open Utils

let parse_input input =
  match Str.(split (regexp "\n\n") input) with
  | [ rules; updates ] ->
      let rules =
        rules |> String.split_lines
        |> List.map ~f:(fun line ->
               match String.split line ~on:'|' with
               | [ l; r ] -> (Int.of_string l, Int.of_string r)
               | _ -> assert false)
      in
      let updates =
        updates |> String.split_lines
        |> List.map ~f:(fun line ->
               String.split ~on:',' line |> List.map ~f:Int.of_string
               |> Array.of_list)
      in
      (rules, updates)
  | _ -> assert false

let find_rule_elements update (l, r) =
  Option.both
    (Array_ex.index_of update l ~equal:Int.equal)
    (Array_ex.index_of update r ~equal:Int.equal)

let check_rule update rule =
  find_rule_elements update rule
  |> Option.map ~f:(fun (li, ri) -> li < ri)
  |> Option.value ~default:true

let part1 rules updates =
  updates
  |> List.filter ~f:(fun update -> List.for_all rules ~f:(check_rule update))
  |> List.map ~f:(fun update -> Array.get update (Array.length update / 2))
  |> List.sum (module Int) ~f:Fn.id

let has_violations update rules =
  List.exists rules ~f:(fun rule -> not (check_rule update rule))

let fix update rules =
  let remaining =
    rules
    |> List.concat_map ~f:(fun (l, r) -> [ l; r ])
    |> Set.of_list (module Int)
  in
  let fixed =
    Array.init (Array.length update) ~f:(fun i ->
        match update.(i) with x when Set.mem remaining x -> None | x -> Some x)
  in
  let rec loop slot remaining edges =
    match slot with
    | i when i >= Array.length fixed -> fixed
    | i when fixed.(i) |> Option.is_some -> loop (slot + 1) remaining edges
    | i ->
        let next =
          Set.find_exn remaining ~f:(fun rem ->
              List.for_all edges ~f:(fun (_, r) -> r <> rem))
        in
        fixed.(i) <- Some next;
        let remaining' = Set.remove remaining next in
        let edges' = List.filter edges ~f:(fun (l, _) -> l <> next) in
        loop (i + 1) remaining' edges'
  in
  loop 0 remaining rules |> Array.map ~f:(fun x -> Option.value_exn x)

let part2 rules updates =
  updates
  |> List.filter_map ~f:(fun upd ->
         let relevant_rules =
           List.filter rules ~f:(fun rule ->
               find_rule_elements upd rule |> Option.is_some)
         in
         if has_violations upd relevant_rules then Some (fix upd relevant_rules)
         else None)
  |> List.map ~f:(fun update -> Array.get update (Array.length update / 2))
  |> List.sum (module Int) ~f:Fn.id

let solve filename =
  let rules, updates = Stdio.In_channel.read_all filename |> parse_input in
  let p1 = part1 rules updates and p2 = part2 rules updates in
  (Some (Int.to_string p1), Some (Int.to_string p2))
