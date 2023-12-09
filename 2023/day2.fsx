#load "utils.fsx"

open System

open System.Text.RegularExpressions
open Utils

let lines = Input.readLines 2 false |> List.ofArray

type Game = int * Map<string, int> list

let input = "Game 1: 19 blue, 12 red; 19 blue, 2 green, 1 red; 13 red, 11 blue"

let parseGame line : Game =
  let m = Regex.Match(line, @"Game (?'id'\d+): (?'rounds'.*)")
  let id = Int32.Parse(m.Groups.["id"].Value)

  let content =
    m.Groups.["rounds"].Value.Split("; ")
    |> Seq.map (fun round ->
      round.Split(", ")
      |> Seq.map (fun cubes ->
        let [| num; col |] = cubes.Split(" ")
        (col, Int32.Parse(num)))
      |> Map)
    |> List.ofSeq

  id, content

let games = List.map parseGame lines

let part1 =
  let capacity = Map [ "red", 12; "green", 13; "blue", 14 ]

  let within ((_, rounds): Game) =
    let outside round =
      round
      |> Map.exists (fun color count ->
        capacity.TryFind color |> Option.defaultValue 0 < count)

    rounds |> Seq.exists outside |> not

  games |> Seq.filter within |> Seq.map (fun (id, _) -> id) |> Seq.sum

let part2 =
  let combineMax m1 m2 =
    Map.fold
      (fun res key (v: int) ->
        Map.add key (max v (Map.tryFind key res |> Option.defaultValue 0)) res)
      m1
      m2

  let minCapacity ((_, rounds): Game) = List.fold combineMax Map.empty rounds

  games
  |> Seq.map minCapacity
  |> Seq.map (fun capacity -> capacity |> Map.values |> Seq.fold (*) 1)
  |> Seq.sum


printfn "Day 2"
printfn "Part 1: %d" part1
printfn "Part 2: %d" part2
