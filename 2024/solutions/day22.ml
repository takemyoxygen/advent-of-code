open Core

let mix = Int.bit_xor
let prune x = x % 16777216

let next_secret secret =
  let secret = mix secret (secret * 64) |> prune in
  let secret = mix secret (secret / 32) |> prune in
  mix secret (secret * 2048) |> prune

let nth_secret ~n init = Fn.apply_n_times ~n next_secret init

let n_secrets ~n init =
  Sequence.unfold ~init:(n, init) ~f:(fun (left, secret) ->
      if left = 0 then None
      else
        let next = next_secret secret in
        Some (secret, (left - 1, next)))
  |> Sequence.to_list

let part1 init_secrets =
  List.sum (module Int) init_secrets ~f:(nth_secret ~n:2000)

let diffs xs =
  let rec loop prev acc = function
    | [] -> List.rev acc
    | x :: rest ->
        let diff = Option.map prev ~f:(fun p -> x - p) in
        loop (Some x) (diff :: acc) rest
  in
  loop None [] xs

let prev_seqs xs =
  let rec loop acc = function
    | a :: (b :: c :: d :: _ as rest) ->
        loop (Option.all [ a; b; c; d ] :: acc) rest
    | _ -> List.rev acc
  in
  let prevs = loop [] xs in
  let prep =
    List.init (List.length xs - List.length prevs) ~f:(Fn.const None)
  in
  prep @ prevs

module Prev_seq = struct
  module T = struct
    type t = int list [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let to_prev_map ~n init_secret =
  let prices = n_secrets ~n init_secret |> List.map ~f:(fun x -> x % 10) in
  let diffs = diffs prices in
  let prev_diffs = prev_seqs diffs in
  List.zip_exn prices prev_diffs
  |> List.filter_map ~f:(fun (price, seq) ->
         Option.map seq ~f:(fun seq -> (seq, price)))
  |> Map.of_alist_reduce (module Prev_seq) ~f:(fun x _ -> x)

let part2 init_secrets =
  let price_maps = init_secrets |> List.map ~f:(to_prev_map ~n:2001) in
  let map_sums =
    List.reduce_exn price_maps
      ~f:(Map.merge_skewed ~combine:(fun ~key:_ a b -> a + b))
  in
  map_sums |> Map.data |> List.max_elt ~compare:Int.compare |> Option.value_exn

let solve filename =
  let init_secrets =
    filename |> In_channel.read_lines |> List.filter_map ~f:Int.of_string_opt
  in
  let p1 = part1 init_secrets and p2 = part2 init_secrets in
  (Some (Int.to_string p1), Some (Int.to_string p2))
