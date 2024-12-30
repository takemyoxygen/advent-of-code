open Core

type registers = { a : int; b : int; c : int } [@@deriving sexp_of]

let combo registers = function
  | x when Int.between x ~low:0 ~high:3 -> x
  | 4 -> registers.a
  | 5 -> registers.b
  | 6 -> registers.c
  | x -> failwith (sprintf "Invalid value %d" x)

let maybe_change f x = match f with Some f -> f x | None -> x

let run program registers =
  let rec loop output registers instr_ptr =
    let seq_op ?a ?b ?c () =
      let registers' =
        {
          a = maybe_change a registers.a;
          b = maybe_change b registers.b;
          c = maybe_change c registers.c;
        }
      in
      loop output registers' (instr_ptr + 2)
    in
    if instr_ptr > Array.length program - 2 then List.rev output
    else
      let instr = program.(instr_ptr) in
      let operand = program.(instr_ptr + 1) in
      match instr with
      | 0 -> seq_op ~a:(fun a -> a / Int.pow 2 (combo registers operand)) ()
      | 1 -> seq_op ~b:(fun b -> Int.bit_xor b operand) ()
      | 2 -> seq_op ~b:(fun _ -> combo registers operand % 8) ()
      | 3 ->
          if registers.a = 0 then loop output registers (instr_ptr + 2)
          else loop output registers operand
      | 4 -> seq_op ~b:(fun b -> Int.bit_xor b registers.c) ()
      | 5 ->
          let next_out = combo registers operand % 8 in
          loop (next_out :: output) registers (instr_ptr + 2)
      | 6 ->
          seq_op
            ~b:(fun _ -> registers.a / Int.pow 2 (combo registers operand))
            ()
      | 7 ->
          seq_op
            ~c:(fun _ -> registers.a / Int.pow 2 (combo registers operand))
            ()
      | _ -> assert false
  in
  loop [] registers 0

let prog_of_str str =
  str |> String.split ~on:',' |> List.map ~f:Int.of_string |> Array.of_list

let part1 registers prog =
  run prog registers |> List.map ~f:Int.to_string |> String.concat ~sep:","

let group_by_first_output program =
  let get_first_out a = run program { a; b = 0; c = 0 } |> List.hd_exn in
  List.range 0 1024
  |> List.map ~f:(fun a -> (get_first_out a, a))
  |> List.Assoc.sort_and_group ~compare:Int.compare

let follow a b = Int.shift_right a 3 = b % 128

let combine_many xs =
  List.foldi xs ~init:0 ~f:(fun i acc x ->
      if i = 0 then x else Int.bit_or (Int.shift_left x (3 * i)) acc)

let run_with_a prog a =
  let registers = { a = 0; b = 0; c = 0 } in
  run prog { registers with a }

let to_binary x =
  let rec loop acc x =
    if x = 0 then acc else loop (Int.to_string (x % 2) ^ acc) (x / 2)
  in
  loop "" x

let part2 prog =
  let outs = group_by_first_output prog |> Map.of_alist_exn (module Int) in
  let rec loop a_parts expected_output =
    match expected_output with
    | [] -> [ a_parts ]
    | out :: rest ->
        let candidates = Map.find_exn outs out in
        let filter =
          match a_parts with [] -> Fn.const true | a :: _ -> follow a
        in
        let possible = List.filter candidates ~f:filter in
        let scenarios =
          List.map possible ~f:(fun new_part -> new_part :: a_parts)
        in
        List.concat_map scenarios ~f:(fun new_parts -> loop new_parts rest)
  in
  let expected_output = List.of_array prog in
  loop [] expected_output
  |> List.map ~f:(fun inputs -> (inputs, combine_many (List.rev inputs)))
  |> List.filter ~f:(fun (_, a) ->
         run_with_a prog a |> List.equal Int.equal expected_output)
  |> List.map ~f:snd
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn

let solve _ =
  let prog = prog_of_str "2,4,1,1,7,5,0,3,4,7,1,6,5,5,3,0" in
  let registers = { a = 25358015; b = 0; c = 0 } in
  let p1 = part1 registers prog in
  let p2 = part2 prog in
  (Some p1, Some (Int.to_string p2))
