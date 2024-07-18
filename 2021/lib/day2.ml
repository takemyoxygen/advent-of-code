type instruction = Forward of int | Up of int | Down of int
type position = { horizontal : int; depth : int; aim : int }

let instructions =
  Stdio.In_channel.read_lines "./input/day2.txt"
  |> List.map (String.split_on_char ' ')
  |> List.map (function
       | [ cmd; arg ] -> (cmd, int_of_string arg)
       | _ -> failwith "Invalid format")
  |> List.map (function
       | "forward", arg -> Forward arg
       | "up", arg -> Up arg
       | "down", arg -> Down arg
       | _ -> failwith "You cannot be")

let apply folder =
  let final =
    List.fold_left folder { horizontal = 0; depth = 0; aim = 0 } instructions
  in
  final.depth * final.horizontal

let part1 =
  apply (fun acc arg ->
      match arg with
      | Forward x -> { acc with horizontal = acc.horizontal + x }
      | Down x -> { acc with depth = acc.depth + x }
      | Up x -> { acc with depth = acc.depth - x })

let part2 =
  apply (fun acc arg ->
      match arg with
      | Forward x ->
          {
            acc with
            horizontal = acc.horizontal + x;
            depth = acc.depth + (acc.aim * x);
          }
      | Down x -> { acc with aim = acc.aim + x }
      | Up x -> { acc with aim = acc.aim - x })
