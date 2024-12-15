open Core

let num_digits x =
  match x with
  | 0L -> 1
  | x ->
      x |> Int64.to_float |> Float.log10 |> Float.round_down |> Int.of_float
      |> ( + ) 1

let transform_stone st =
  match (st, num_digits st) with
  | 0L, _ -> [ 1L ]
  | st, dig when dig % 2 = 0 ->
      let div = Int.(10 ** (dig / 2)) |> Int64.of_int in
      let left = Int64.(st / div) and right = Int64.(st % div) in
      [ left; right ]
  | _ -> [ Int64.(st * 2024L) ]

let blink2 stone_counts =
  stone_counts
  |> List.concat_map ~f:(fun (st, count) ->
         transform_stone st |> List.map ~f:(fun st' -> (st', count)))
  |> List.Assoc.sort_and_group ~compare:Int64.compare
  |> List.Assoc.map ~f:(List.sum (module Int) ~f:Fn.id)

let blink_n stone_counts n =
  Fn.apply_n_times ~n blink2 stone_counts |> List.sum (module Int) ~f:snd

let solve filename =
  let input =
    filename |> Stdio.In_channel.read_all |> String.strip
    |> String.split ~on:' '
    |> List.map ~f:Int64.of_string
    |> List.map ~f:(fun st -> (st, 1))
  in
  let p1 = blink_n input 25 and p2 = blink_n input 75 in
  (Some (Int.to_string p1), Some (Int.to_string p2))
