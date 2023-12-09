#load "utils.fsx"

open System

module Seq =
  let all pred xs = xs |> Seq.exists (pred >> not) |> not

let lines = Utils.readLines 9 false

let input =
  lines
  |> Seq.map (fun line -> line.Split(" ") |> Seq.map Int32.Parse |> List.ofSeq)
  |> List.ofSeq

let part1 () =
  let rec complete xs =
    if Seq.all ((=) 0) xs then
      0
    else
      let diffs = List.pairwise xs |> List.map (fun (a, b) -> b - a)
      (complete diffs) + (List.last xs)

  input |> Seq.map complete |> Seq.sum

let part2 () =
  let rec prepend xs =
    if Seq.all ((=) 0) xs then
      0
    else
      let diffs = List.pairwise xs |> List.map (fun (a, b) -> b - a)
      (List.head xs) - (prepend diffs)

  input |> Seq.map prepend |> Seq.sum


printfn "Day 9"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
