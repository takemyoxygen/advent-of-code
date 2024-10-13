open Core

type operand = Variable of string | Literal of int
type apply_spec = { input : int list; z : int }

let read_file filename =
  let parse_operand op =
    match Int.of_string_opt op with Some d -> Literal d | None -> Variable op
  in
  Stdio.In_channel.read_lines filename
  |> List.map ~f:(fun line ->
         match String.split ~on:' ' line with
         | [ "inp"; a ] -> `inp (Variable a)
         | [ "add"; a; b ] -> `add (parse_operand a, parse_operand b)
         | [ "mul"; a; b ] -> `mul (parse_operand a, parse_operand b)
         | [ "div"; a; b ] -> `div (parse_operand a, parse_operand b)
         | [ "mod"; a; b ] -> `modulo (parse_operand a, parse_operand b)
         | [ "eql"; a; b ] -> `eql (parse_operand a, parse_operand b)
         | _ -> assert false)

let digits = List.range 1 10

let classify_group = function
  | [
      `inp _;
      `mul (_, _);
      `add (_, _);
      `modulo (_, _);
      `div (Variable "z", Literal ((1 | 26) as div_z));
      `add (Variable "x", Literal add_x);
      `eql (_, _);
      `eql (_, _);
      `mul (_, _);
      `add (_, _);
      `mul (_, _);
      `add (_, _);
      `mul (_, _);
      `mul (_, _);
      `add (_, _);
      `add (Variable "y", Literal add_y);
      `mul (_, _);
      `add (_, _);
    ] ->
      (div_z, add_x, add_y)
  | _ -> failwith "Unexpected group format"

let reverse_apply instr_group z =
  let div_z, add_x, add_y = classify_group instr_group in
  List.filter_map digits ~f:(fun w ->
      let special_rem = w - add_x in
      let z' =
        match div_z with
        | 26 -> Some ((z * 26) + special_rem)
        | 1 ->
            let num = z - w - add_y in
            if num % 26 = 0 then Some (num / 26) else None
        | _ -> assert false
      in
      Option.map z' ~f:(fun z' -> (w, z')))

let reverse_apply_state instr_group (state : apply_spec) : apply_spec list =
  reverse_apply instr_group state.z
  |> List.map ~f:(fun (inp, z) -> { z; input = inp :: state.input })

let reverse_apply_groups instr_groups =
  instr_groups
  |> List.fold
       ~init:[ { input = []; z = 0 } ]
       ~f:(fun states group ->
         let states' =
           states |> List.concat_map ~f:(reverse_apply_state group)
         in
         states')

let solve filename =
  let to_str digits =
    List.map digits ~f:Int.to_string |> String.concat ~sep:""
  in
  let compare = [%compare: int list] in
  let instr_groups =
    read_file filename
    |> List.group ~break:(fun _ b -> match b with `inp _ -> true | _ -> false)
    |> List.rev
  in
  let inputs =
    reverse_apply_groups instr_groups
    |> List.filter ~f:(fun st -> st.z = 0)
    |> List.map ~f:(fun st -> st.input)
  in
  let part1 = List.max_elt inputs ~compare |> Option.value_exn |> to_str
  and part2 = List.min_elt inputs ~compare |> Option.value_exn |> to_str in
  (Some part1, Some part2)
