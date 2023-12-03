#load "utils.fsx"

open System
open System.Text.RegularExpressions

type Num = { Start: int; Length: int; Value: int }

let offsets = [ 0, 1; 0, -1; 1, 0; -1, 0; 1, 1; 1, -1; -1, 1; -1, -1 ]

let input = Utils.readLines 3 false

let extractNumbers line =
  Regex.Matches(line, "\d+")
  |> Seq.map (fun f ->
    { Start = f.Index
      Length = f.Length
      Value = Int32.Parse(f.Value) })
  |> List.ofSeq

let numbers = Array.map extractNumbers input


let adjacentTo (input: string array) line col =
  offsets
  |> Seq.map (fun (dx, dy) -> line + dy, col + dx)
  |> Seq.filter (fun (l, c) ->
    l >= 0 && c >= 0 && l < input.Length && c < input.[line].Length)

let part1 () =
  let isPartNumber input line (number: Num) =
    seq { number.Start .. (number.Start + number.Length - 1) }
    |> Seq.collect (adjacentTo input line)
    |> Seq.exists (fun (l, c) ->
      match input.[l].[c] with
      | c when Char.IsDigit(c) || c = '.' -> false
      | _ -> true)

  numbers
  |> Seq.mapi (fun line nums -> Seq.filter (isPartNumber input line) nums)
  |> Seq.concat
  |> Seq.sumBy _.Value

let part2 () =
  let adjacentNumbers input (nums: Num list array) line col =
    adjacentTo input line col
    |> Seq.collect (fun (l, c) ->
      Seq.filter (fun n -> n.Start <= c && c < n.Start + n.Length) nums.[l])
    |> Seq.distinct


  input
  |> Seq.mapi (fun line row ->
    row
    |> Seq.indexed
    |> Seq.filter (fun (_, sym) -> sym = '*')
    |> Seq.map fst
    |> Seq.map (fun c ->
      match adjacentNumbers input numbers line c |> List.ofSeq with
      | [ a; b ] -> a.Value * b.Value
      | _ -> 0))
  |> Seq.concat
  |> Seq.sum


printfn "Day 3"
printfn "Part 1: %d" <| part1 ()
printfn "Part 2: %d" <| part2 ()
