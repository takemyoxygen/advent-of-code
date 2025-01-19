open Core

let parse_input filename =
  let text = In_channel.read_all filename |> String.strip in
  match Str.split (Str.regexp "\n\n") text with
  | [ initial; connections ] ->
      let initial =
        initial |> String.split_lines
        |> List.map ~f:(fun line ->
               match String.split line ~on:':' with
               | [ wire; init_value ] ->
                   (wire, init_value |> String.strip |> Int.of_string)
               | _ -> assert false)
        |> Map.of_alist_exn (module String)
      in
      let connections =
        connections |> String.split_lines
        |> List.map ~f:(fun line ->
               match String.split ~on:' ' line with
               | [ w1; op; w2; _; res ] -> (w1, op, w2, res)
               | _ -> assert false)
      in
      (initial, connections)
  | _ -> assert false

let eval = function
  | "AND" -> Int.bit_and
  | "OR" -> Int.bit_or
  | "XOR" -> Int.bit_xor
  | _ -> assert false

let step wire_values connections =
  List.fold connections ~init:wire_values
    ~f:(fun wire_values (w1, op, w2, res) ->
      let v1 = Map.find wire_values w1 and v2 = Map.find wire_values w2 in
      match Option.both v1 v2 with
      | Some (v1, v2) -> Map.set wire_values ~key:res ~data:(eval op v1 v2)
      | None -> wire_values)

let all_wires_initialized wires wire_values =
  List.for_all wires ~f:(Map.mem wire_values)

let repeat_until_all_z initial connections =
  let zs =
    List.map connections ~f:(fun (_, _, _, res) -> res)
    |> List.filter ~f:(String.is_prefix ~prefix:"z")
    |> List.dedup_and_sort ~compare:String.compare
  in
  let rec loop wire_values =
    if all_wires_initialized zs wire_values then wire_values
    else loop (step wire_values connections)
  in
  (zs, loop initial)

let part1 initial connections =
  let zs, wire_values = repeat_until_all_z initial connections in
  List.map zs ~f:(Map.find_exn wire_values)
  |> List.rev |> List.map ~f:Int.to_string |> String.concat
  |> fun bin -> Int.of_string ("0b" ^ bin)

let swap c1 c2 connections =
  List.map connections ~f:(fun ((w1, op, w2, c) as conn) ->
      if String.equal c c1 then (w1, op, w2, c2)
      else if String.equal c c2 then (w1, op, w2, c1)
      else conn)

let find_invalid_bits initial_carry additions =
  let rec loop carry_from_prev agg left =
    match left with
    | [] -> agg
    | (i, prev_carry, a, b, c, carry, result) :: rest ->
        let agg' =
          match Option.all [ prev_carry; a; b; c; carry; result ] with
          | Some _ -> agg
          | None -> i :: agg
        in
        let agg'' =
          match (carry_from_prev, prev_carry) with
          | Some carry_from_prev, Some prev_carry
            when not (String.equal carry_from_prev prev_carry) ->
              (i - 1) :: agg'
          | _ -> agg'
        in
        loop carry agg'' rest
  in
  loop initial_carry [] (List.of_array additions) |> Set.of_list (module Int)

let wires_to_permute initial_carry additions =
  let invalid_bits = find_invalid_bits initial_carry additions in
  invalid_bits |> Set.to_list
  |> List.map ~f:(fun bit ->
         let _, pcr, a, b, c, cr, r = additions.(bit - 1) in
         [ pcr; a; b; c; cr; r ] |> List.filter_map ~f:Fn.id)

let output (_, _, _, x) = x

let find_conn connections w1 op w2 =
  List.find connections ~f:(fun (cw1, cop, cw2, _) ->
      String.(cop = op && ((cw2 = w2 && cw1 = w1) || (cw2 = w1 && cw1 = w2))))
  |> Option.map ~f:output

let form_additions connections =
  let x i = sprintf "x%02d" i in
  let y i = sprintf "y%02d" i in
  let find_conn = find_conn connections in
  let find_operand another op result =
    let result_pred =
      match result with None -> Fn.const true | Some res -> String.equal res
    in
    List.find_map connections ~f:(fun (cw1, cop, cw2, r) ->
        match String.(cop = op) && result_pred r with
        | true when String.equal another cw1 -> Some (cw2, r)
        | true when String.equal another cw2 -> Some (cw1, r)
        | _ -> None)
  in
  let max_input_bit = 44 in
  Array.init max_input_bit ~f:(fun i ->
      let i = i + 1 in
      let x = x i and y = y i in
      let a = find_conn x "XOR" y and b = find_conn x "AND" y in
      let prev_carry, result =
        Option.bind a ~f:(fun a -> find_operand a "XOR" None) |> function
        | None -> (None, None)
        | Some (c, r) -> (Some c, Some r)
      in
      let c =
        Option.both prev_carry a
        |> Option.bind ~f:(fun (prev_carry, a) -> find_conn prev_carry "AND" a)
      in
      let carry =
        Option.both b c |> Option.bind ~f:(fun (b, c) -> find_conn b "OR" c)
      in
      (i, prev_carry, a, b, c, carry, result))

let gen_pairs wires =
  let rec loop acc wires =
    match wires with
    | x :: rest ->
        let pairs = List.map rest ~f:(fun r -> (x, r)) in
        loop (acc @ pairs) rest
    | [] -> acc
  in
  loop [] wires

(*
Bit addition looks like:
inputs - x, y and prev_carry:

a = x XOR y
b = x AND y
c = prev_carry AND a
carry = b OR c
z = a XOR prev_carry

so finding bits where this format doesn't match
and tryint to permute wires in thouse
*)

let part2 connections =
  let carry0 = find_conn connections "x00" "AND" "y00" in
  let additions = form_additions connections in
  let wires = wires_to_permute carry0 additions in
  List.fold wires
    ~init:(Sequence.singleton ([], connections))
    ~f:(fun acc wires ->
      let%bind.Sequence permuted, connections = acc in
      let%map.Sequence w1, w2 = Sequence.of_list (gen_pairs wires) in
      (w1 :: w2 :: permuted, swap w1 w2 connections))
  |> Sequence.find_exn ~f:(fun (_, connections) ->
         connections |> form_additions |> find_invalid_bits carry0
         |> Set.is_empty)
  |> fst
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:","

let solve filename =
  let initial_values, connections = parse_input filename in
  let p1 = part1 initial_values connections in
  let p2 = part2 connections in
  (Some (Int.to_string p1), Some p2)
