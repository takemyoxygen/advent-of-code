#load "utils.fsx"

open System
open Utils

let lines = Input.readLines 4 false

let parse (line: string) =
  let [| _; nums |] = line.Split(": ")
  let [| winning; mine |] = nums.Split(" | ")

  let extractNums (s: string) =
    s.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int32.Parse

  winning |> extractNums |> set, mine |> extractNums |> set

let cards = Array.map parse lines

let part1 () =
  cards
  |> Seq.map (fun (winning, mine) -> Set.intersect winning mine |> Seq.length)
  |> Seq.map (fun n ->
    if n = 0 then 0 else Math.Pow(2.0, (float n - 1.0)) |> int)
  |> Seq.sum

let part2 () =
  let cardCounts = Array.create cards.Length 1

  cards
  |> Seq.map (fun (winning, mine) -> Set.intersect winning mine |> Seq.length)
  |> Seq.iteri (fun i wins ->
    for card in seq { 1..wins } do
      cardCounts.[i + card] <- cardCounts.[i + card] + cardCounts.[i])

  Seq.sum cardCounts


printfn "Day 4"
printfn "Part 1: %d" <| part1 ()
printfn "Part 2: %d" <| part2 ()
