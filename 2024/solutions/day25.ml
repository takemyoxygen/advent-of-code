open! Core

let parse_input filename =
  let count_hashes lines column =
    lines
    |> List.map ~f:(fun line -> String.get line column)
    |> List.count ~f:(Char.equal '#')
  in
  filename |> In_channel.read_all |> String.strip
  |> Str.split (Str.regexp "\n\n")
  |> List.map ~f:(fun gr ->
         let lines = String.split_lines gr in
         let kind =
           match List.hd lines with
           | Some "#####" -> `Lock
           | Some "....." -> `Key
           | _ -> assert false
         in
         let heights =
           List.range 0 5
           |> List.map ~f:(count_hashes lines)
           |> List.map ~f:(fun h -> h - 1)
         in
         (kind, heights))
  |> List.fold ~init:([], []) ~f:(fun (keys, locks) (kind, heights) ->
         match kind with
         | `Key -> (heights :: keys, locks)
         | `Lock -> (keys, heights :: locks))

let key_fit lock key =
  List.zip_exn lock key |> List.for_all ~f:(fun (l, k) -> l + k <= 5)

let solve filename =
  let keys, locks = parse_input filename in
  let p1 =
    List.cartesian_product keys locks
    |> List.count ~f:(fun (key, lock) -> key_fit lock key)
  in
  (Some (Int.to_string p1), None)
