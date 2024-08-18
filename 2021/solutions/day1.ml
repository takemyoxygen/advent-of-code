let numbers =
  Stdio.In_channel.read_lines "./input/day1.txt" |> List.map int_of_string

let pairwise xs =
  Seq.unfold
    (function f :: s :: rest -> Some ((f, s), s :: rest) | _ -> None)
    xs

let sums_of_3 xs =
  Seq.unfold
    (function
      | a :: b :: c :: rest -> Some (a + b + c, b :: c :: rest) | _ -> None)
    xs

let num_of_increasing pairs =
  pairs |> Seq.filter (fun (x1, x2) -> x2 > x1) |> Seq.length

let part1 () = numbers |> pairwise |> num_of_increasing |> string_of_int

let part2 () =
  numbers |> sums_of_3 |> List.of_seq |> pairwise |> num_of_increasing
  |> string_of_int
