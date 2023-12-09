#load "utils.fsx"

open System
open System.Text.RegularExpressions
open Utils

let lines = Input.readLines 1 false

let digits =
  Map
    [ ("one", "1")
      ("two", "2")
      ("three", "3")
      ("four", "4")
      ("five", "5")
      ("six", "6")
      ("seven", "7")
      ("eight", "8")
      ("nine", "9") ]

let regex = String.Format("(\d|{0})", String.Join("|", Map.keys digits))

let digitStringToNumber s =
  match s, Map.tryFind s digits with
  | _ when s.Length = 1 && Char.IsDigit(s.[0]) -> s
  | _, Some(d) -> d
  | _ -> failwithf "Unexpected digit value: %s" s

let extractNumbersV2 line =
  [| Regex.Match(line, regex).Value
     Regex.Match(line, regex, RegexOptions.RightToLeft).Value |]
  |> Array.map digitStringToNumber


let extractNumbersV1 line =
  line |> Seq.filter Char.IsDigit |> Seq.map _.ToString() |> Array.ofSeq


let solve (extractNumbers: (string -> string array)) =
  lines
  |> Seq.map extractNumbers
  |> Seq.map (fun numbers -> numbers.[0] + (Array.last numbers))
  |> Seq.map Int32.Parse
  |> Seq.sum


printfn "Day 1"
printfn "Part 1: %d" (solve extractNumbersV1)
printfn "Part 2: %d" (solve extractNumbersV2)
