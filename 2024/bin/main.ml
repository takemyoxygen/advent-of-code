open Base
open Core
open Solutions

module type Day = sig
  val solve : string -> string option * string option
end

let solutions : (module Day) Int.Map.t =
  [
    (1, (module Day1 : Day));
    (2, (module Day2));
    (3, (module Day3));
    (4, (module Day4));
    (5, (module Day5));
    (6, (module Day6));
  ]
  |> Map.of_alist_exn (module Int)

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
       let sln = Map.find_exn solutions day in
       let start = Time_now.nanosecond_counter_for_timing () in
       print day sln input_file;
       let fin = Time_now.nanosecond_counter_for_timing () in
       let elapsed = Int63.(to_float (fin - start)) in
       printf "Elapsed time: %f sec\n" Float.(elapsed / 1000000000.0))

let () = Command_unix.run command
