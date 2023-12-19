#load "utils.fsx"

open Utils
open System
open System.Collections.Generic

let lines = Input.readLines 12 false

let input =
  lines
  |> Seq.map (fun line ->
    let [| symbols; damages |] = line.Split(' ')
    symbols, (damages.Split(',') |> Seq.map Int32.Parse |> List.ofSeq))


let canHaveDamagedInterval (symbols: string) start size =
  if start + size > symbols.Length then
    false
  else
    let nextSym = String.tryAt (start + size) symbols
    let nextOp = symbols.IndexOf('.', start + 1)

    nextSym <> Some('#') && (nextOp = -1 || nextOp >= start + size)


let rec countArrangements (input: string) damages =
  let cache = new Dictionary<int * int list, bigint>()

  let rec loop start damages =
    match String.tryAt start input, damages with
    | None, [] -> 1I
    | None, _ -> 0I
    | Some('.'), _ -> cacheLoop (start + 1) damages
    | Some('#'), [] -> 0I
    | Some('#'), dmg :: rest ->
      if canHaveDamagedInterval input start dmg then
        cacheLoop (start + dmg + 1) rest
      else
        0I
    | Some('?'), dmg :: rest ->
      (cacheLoop (start + 1) damages)
      + (if canHaveDamagedInterval input start dmg then
           cacheLoop (start + dmg + 1) rest
         else
           0I)
    | Some('?'), [] -> if input.IndexOf('#', start + 1) = -1 then 1I else 0I
    | x -> failwithf "Unexpected pattern %A" x

  and cacheLoop start damages =
    match cache.TryGetValue <| (start, damages) with
    | true, x -> x
    | false, _ ->
      let res = loop start damages
      cache.Add((start, damages), res)
      res

  loop 0 damages


let part1 () =
  input
  |> Seq.map (fun (symbols, damages) -> countArrangements symbols damages)
  |> Seq.sum

let part2 () =
  input
  |> Seq.map (fun (symbols, damages) ->
    String.Join('?', Seq.replicate 5 symbols), List.repeat 5 damages)
  |> Seq.map (fun (symbols, damages) -> countArrangements symbols damages)
  |> Seq.sum

printfn "Day 12"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
