open Base
open Core

type operator_type = Sum | Product | Min | Max | Gt | Lt | Eq

type packet = { version : int; content : packet_content }
and packet_content = Literal of int | Operator of operator_type * packet list

let input = Stdio.In_channel.read_all "./input/day16.txt"

let mapping =
  [
    ('0', [ 0; 0; 0; 0 ]);
    ('1', [ 0; 0; 0; 1 ]);
    ('2', [ 0; 0; 1; 0 ]);
    ('3', [ 0; 0; 1; 1 ]);
    ('4', [ 0; 1; 0; 0 ]);
    ('5', [ 0; 1; 0; 1 ]);
    ('6', [ 0; 1; 1; 0 ]);
    ('7', [ 0; 1; 1; 1 ]);
    ('8', [ 1; 0; 0; 0 ]);
    ('9', [ 1; 0; 0; 1 ]);
    ('A', [ 1; 0; 1; 0 ]);
    ('B', [ 1; 0; 1; 1 ]);
    ('C', [ 1; 1; 0; 0 ]);
    ('D', [ 1; 1; 0; 1 ]);
    ('E', [ 1; 1; 1; 0 ]);
    ('F', [ 1; 1; 1; 1 ]);
  ]
  |> Map.of_alist_exn (module Char)

let hex_to_binary str =
  String.to_list str |> List.bind ~f:(Map.find_exn mapping)

let binary_to_decimal bits =
  bits |> List.rev
  |> List.mapi ~f:(fun idx v -> (idx, v))
  |> List.fold ~init:0 ~f:(fun acc (pow, v) -> acc + (v * Int.pow 2 pow))

let parse_literal bits =
  let rec loop acc bits =
    let chunk_bits, rest = List.split_n bits 5 in
    let num_chunk = List.tl_exn chunk_bits in
    let acc' = List.append acc num_chunk in
    match chunk_bits with
    | 1 :: _ -> loop acc' rest
    | 0 :: _ -> (acc', rest)
    | _ -> assert false
  in
  let num_bits, rest = loop [] bits in
  (Literal (binary_to_decimal num_bits), rest)

let get_type = function
  | 0 -> Sum
  | 1 -> Product
  | 2 -> Min
  | 3 -> Max
  | 5 -> Gt
  | 6 -> Lt
  | 7 -> Eq
  | _ -> assert false

let rec parse_operator_subpackets bits =
  let head_bits, bits = List.split_n bits 1 in
  match head_bits with
  | [ 0 ] ->
      let length_bits, bits = List.split_n bits 15 in
      let length = binary_to_decimal length_bits in
      let subpacket_bits, rest = List.split_n bits length in
      let subpackets =
        Sequence.unfold ~init:subpacket_bits ~f:(function
          | [] -> None
          | bits -> Some (parse_packet bits))
        |> Sequence.to_list
      in
      (subpackets, rest)
  | [ 1 ] ->
      let number_bits, bits = List.split_n bits 11 in
      let number = binary_to_decimal number_bits in
      let subpackets, rest =
        List.range 0 number
        |> List.fold ~init:([], bits) ~f:(fun (subpackets, bits) _ ->
               let packet, rest = parse_packet bits in
               (packet :: subpackets, rest))
      in
      (List.rev subpackets, rest)
  | _ -> assert false

and parse_packet bits =
  let ver_bits, bits = List.split_n bits 3 in
  let type_bits, bits = List.split_n bits 3 in
  let version = binary_to_decimal ver_bits in
  match binary_to_decimal type_bits with
  | 4 ->
      let literal, rest = parse_literal bits in
      ({ version; content = literal }, rest)
  | x ->
      let op_type = get_type x in
      let subpackets, rest = parse_operator_subpackets bits in
      ({ version; content = Operator (op_type, subpackets) }, rest)

let rec sum_versions packet =
  match packet.content with
  | Literal _ -> packet.version
  | Operator (_, subpackets) ->
      let sub_sum =
        List.map subpackets ~f:sum_versions |> List.reduce_exn ~f:( + )
      in
      sub_sum + packet.version

let rec eval packet =
  match packet.content with
  | Literal l -> l
  | Operator (op, subs) -> (
      match (op, List.map subs ~f:eval) with
      | Sum, inner -> List.reduce_exn inner ~f:( + )
      | Product, inner -> List.reduce_exn inner ~f:( * )
      | Min, inner ->
          List.min_elt inner ~compare:Int.compare |> Option.value_exn
      | Max, inner ->
          List.max_elt inner ~compare:Int.compare |> Option.value_exn
      | Gt, [ fst; snd ] -> if fst > snd then 1 else 0
      | Lt, [ fst; snd ] -> if fst < snd then 1 else 0
      | Eq, [ fst; snd ] -> if fst = snd then 1 else 0
      | _ -> assert false)

let part1 () =
  let bits = hex_to_binary input in
  let packet, _ = parse_packet bits in
  sum_versions packet

let part2 () =
  let bits = hex_to_binary input in
  let packet, _ = parse_packet bits in
  eval packet
