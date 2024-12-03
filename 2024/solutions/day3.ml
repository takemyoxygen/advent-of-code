open Core

let num = Re.(seq [ digit; opt digit; opt digit ])

let regex =
  Re.compile
    Re.(
      alt
        [
          seq [ str "mul("; group num; str ","; group num; str ")" ];
          str "do()";
          str "don't()";
        ])

let parse filename =
  Stdio.In_channel.read_all filename
  |> Re.all regex
  |> List.map ~f:(fun gr ->
         match Re.Group.get gr 0 with
         | s when String.is_prefix s ~prefix:"mul" ->
             `Mul
               ( Int.of_string (Re.Group.get gr 1),
                 Int.of_string (Re.Group.get gr 2) )
         | "do()" -> `Do
         | "don't()" -> `Dont
         | s -> failwith (sprintf "unknown: %s" s))

let part1 instructions =
  List.fold instructions ~init:0 ~f:(fun acc instr ->
      match instr with `Mul (a, b) -> acc + (a * b) | _ -> acc)

let part2 instructions =
  instructions
  |> List.fold ~init:(0, true) ~f:(fun (acc, enabled) instr ->
         match (enabled, instr) with
         | true, `Mul (a, b) -> (acc + (a * b), true)
         | false, `Mul _ -> (acc, false)
         | _, `Do -> (acc, true)
         | _, `Dont -> (acc, false))
  |> fst

let solve filename =
  let instructions = parse filename in
  let p1 = part1 instructions and p2 = part2 instructions in
  (Some (Int.to_string p1), Some (Int.to_string p2))
