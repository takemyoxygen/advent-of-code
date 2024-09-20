open Core
open Solutions

module type Day = sig
  val solve : string -> string option * string option
end

let solutions : (module Day) array =
  [|
    (module Day1); (module Day2); (module Day3); (module Day4); (module Day5);
  |]

let print day (module D : Day) input_file =
  let p1, p2 = D.solve input_file in
  let res p = Option.value p ~default:"n/a" in
  printf "Day %d\nPart1: %s\nPart2: %s\n" day (res p1) (res p2)

let determine_input_file day is_test explicit_file =
  match explicit_file with
  | Some f -> f
  | None ->
      let postfix = if is_test then "-test" else "" in
      sprintf "./input/day%d%s.txt" day postfix

let command =
  Command.basic ~summary:"Runs solutions for Advent of Code 2021"
    (let%map_open.Command day = anon ("day" %: int)
     and is_test = flag "--test" no_arg ~doc:"Use test file"
     and input =
       flag "--input"
         (optional Filename_unix.arg_type)
         ~doc:"File to use as input"
     in
     fun () ->
       let input_file = determine_input_file day is_test input in
       let sln = solutions.(day - 1) in
       print day sln input_file)

let () = Command_unix.run command
