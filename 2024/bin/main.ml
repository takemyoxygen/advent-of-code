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
    (7, (module Day7));
    (8, (module Day8));
    (9, (module Day9));
    (10, (module Day10));
    (11, (module Day11));
    (12, (module Day12));
    (13, (module Day13));
    (14, (module Day14));
    (15, (module Day15));
    (16, (module Day16));
    (17, (module Day17));
    (18, (module Day18));
    (19, (module Day19));
    (20, (module Day20));
    (22, (module Day22));
    (23, (module Day23));
    (24, (module Day24));
    (25, (module Day25));
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
  Command.basic ~summary:"Runs solutions for Advent of Code 2024"
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
