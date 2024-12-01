open Core

let read_file filename =
  let pairs =
    Stdio.In_channel.read_lines filename
    |> List.map ~f:(fun line ->
           line |> String.split ~on:' '
           |> List.filter ~f:(fun s -> not (String.is_empty s)))
    |> List.map ~f:(function
         | [ a; b ] -> (Int.of_string a, Int.of_string b)
         | _ -> assert false)
  in
  (List.map pairs ~f:fst, List.map pairs ~f:snd)

let part1 (lefts, rights) =
  let lefts = List.sort lefts ~compare:Int.compare
  and rights = List.sort rights ~compare:Int.compare in
  List.map2_exn lefts rights ~f:(fun x y -> abs (x - y))
  |> List.sum (module Int) ~f:Fn.id

let part2 (lefts, rights) =
  let counts =
    rights
    |> List.sort_and_group ~compare:Int.compare
    |> List.map ~f:(fun gr -> (List.hd_exn gr, List.length gr))
    |> Map.of_alist_exn (module Int)
  in
  lefts
  |> List.sum
       (module Int)
       ~f:(fun x -> x * (Map.find counts x |> Option.value ~default:0))

let solve filename =
  let input = read_file filename in
  (Some (Int.to_string (part1 input)), Some (Int.to_string (part2 input)))
