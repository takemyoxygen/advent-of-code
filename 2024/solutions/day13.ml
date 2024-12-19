open! Core

let parse input =
  let num_regex = Re.compile (Re.Pcre.re "\\d+") in
  let extract_nums line =
    let nums = Re.matches num_regex line |> List.map ~f:Float.of_string in
    match nums with [ x; y ] -> (x, y) | _ -> failwith "Invalid input"
  in
  Str.split (Str.regexp "\n\n") input
  |> List.map ~f:(fun gr ->
         match gr |> String.split_lines |> List.map ~f:extract_nums with
         | [ a; b; p ] -> (a, b, p)
         | _ -> failwith "Invalid input")

let looks_like_positive_int flt =
  if
    Float.round_decimal flt ~decimal_digits:3 |> Float.equal (Float.round flt)
    && Float.(flt > 0.0)
  then Some (Int.of_float flt)
  else None

let solve (ax, ay) (bx, by) (px, py) =
  let b = ((ax *. py) -. (ay *. px)) /. ((ax *. by) -. (ay *. bx)) in
  let a = (px -. (b *. bx)) /. ax in
  Option.both (looks_like_positive_int a) (looks_like_positive_int b)
  |> Option.map ~f:(fun (a, b) -> ((a * 3) + b, a, b))

let part1 tasks =
  tasks
  |> List.sum
       (module Int)
       ~f:(fun (a, b, p) ->
         match solve a b p with
         | Some (score, a, b) when a <= 100 && b <= 100 -> score
         | _ -> 0)

let part2 tasks =
  tasks
  |> List.sum
       (module Int)
       ~f:(fun (a, b, (px, py)) ->
         let p = (px +. 10000000000000., py +. 10000000000000.) in
         match solve a b p with Some (score, _, _) -> score | _ -> 0)

let solve filename =
  let tasks = filename |> Stdio.In_channel.read_all |> String.strip |> parse in
  let p1 = part1 tasks and p2 = part2 tasks in
  (Some (Int.to_string p1), Some (Int.to_string p2))
