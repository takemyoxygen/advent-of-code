#load "utils.fsx"

open System
open System.Linq
open System.Text
open System.Collections.Specialized
open Utils
open System.Collections

type Instruction =
  | Remove
  | Set of int

let [| line |] = Input.readLines 15 false
let instructions = line.Split(',')

let hash (s: string) =
  Encoding.ASCII.GetBytes(s)
  |> Seq.fold (fun acc byte -> ((acc + (int byte)) * 17) % 256) 0

let part1 () = instructions |> Seq.sumBy hash

let part2 () =
  let boxes = Array.zeroCreate 256
  Array.iteri (fun i _ -> boxes[i] <- new OrderedDictionary()) boxes

  instructions
  |> Seq.map (fun s ->
    let remIdx = s.IndexOf('-')
    let setIdx = s.IndexOf('=')
    let label = s.Substring(0, max remIdx setIdx)

    if remIdx >= 0 then
      label, Remove
    else
      let flength = s.Substring(setIdx + 1) |> Int32.Parse
      label, Set flength)
  |> Seq.iter (fun (label, instr) ->
    let lenses = boxes[hash label]

    match instr with
    | Remove -> lenses.Remove(label)
    | Set(fpower) -> lenses[label] <- fpower)

  boxes
  |> Seq.mapi (fun boxIdx lenses ->
    lenses.Cast<DictionaryEntry>()
    |> Seq.mapi (fun lenseIdx entry ->
      (boxIdx + 1) * (lenseIdx + 1) * (entry.Value :?> int))
    |> Seq.sum)
  |> Seq.sum

printfn "Day 15"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
