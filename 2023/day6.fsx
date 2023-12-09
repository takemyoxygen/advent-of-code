#load "utils.fsx"

open System
open Utils

let lines = Input.readLines 6 false

let solve a b c =
  let d = b * b - 4I * a * c
  let x2 = (-(float b) + Math.Sqrt(float d)) / (2.0 * (float a))
  let x1 = (-(float b) - Math.Sqrt(float d)) / (2.0 * (float a))
  min x1 x2, max x1 x2

let ways (time: bigint) (dist: bigint) =
  let x1, x2 = solve -1I time -dist
  Math.Ceiling(x2 - 1.0) - Math.Floor(x1 + 1.0) + 1.0 |> bigint

let part1 () =
  let input =
    let [ times; distances ] =
      lines
      |> Seq.map (fun line ->
        line.Split(" ", StringSplitOptions.RemoveEmptyEntries)
        |> Seq.skip 1
        |> Seq.map bigint.Parse)
      |> List.ofSeq

    Seq.zip times distances |> List.ofSeq

  input |> Seq.map (fun (time, dist) -> ways time dist) |> Seq.fold (*) 1I


let part2 () =
  let [ time; distance ] =
    lines
    |> Seq.map (fun line -> line.Split(":") |> Array.last |> _.Replace(" ", ""))
    |> Seq.map bigint.Parse
    |> List.ofSeq

  ways time distance


printfn "Day 6"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
