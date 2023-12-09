#load "utils.fsx"

open System
open System.Text.RegularExpressions
open Utils

type NodeMap = Map<string, string * string>

let lines = Input.readLines 8 false |> List.ofArray

let instructions, map =
  let instructions :: _ :: rest = lines

  let map =
    rest
    |> Seq.map (fun line ->
      let [ src; left; right ] =
        Regex.Matches(line, "[0-9A-Z]{3}") |> Seq.map _.Value |> List.ofSeq

      src, (left, right))
    |> Map

  instructions |> List.ofSeq, map


let findSteps instructions (map: NodeMap) isFinish start =
  let rec loop node instructionsLeft steps =
    match node, instructionsLeft with
    | n, _ when isFinish n -> steps
    | n, 'L' :: rest -> loop (map.[n] |> fst) rest (steps + 1)
    | n, 'R' :: rest -> loop (map.[n] |> snd) rest (steps + 1)
    | n, [] -> loop n instructions steps
    | _ -> failwithf "Unknown instructions: %A" instructionsLeft

  loop start instructions 0



let part1 () =
  findSteps instructions map ((=) "ZZZ") "AAA"


let part2 () =
  let rec gcd a b =
    match (a, b) with
    | (x, y) when x = y -> x
    | (x, y) when x > y -> gcd (x - y) y
    | (x, y) -> gcd x (y - x)

  let lcm a b = a * b / (gcd a b)

  map.Keys
  |> Seq.filter _.EndsWith('A')
  |> Seq.map (findSteps instructions map _.EndsWith('Z'))
  |> Seq.map bigint
  |> Seq.fold lcm 1I


printfn "Day 8"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
