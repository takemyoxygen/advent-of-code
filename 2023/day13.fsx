#load "utils.fsx"

open Utils

type ReflectionLine =
  | Col of int
  | Row of int

let lines = Input.readLines 13 false

let input =
  let readGrid lines =
    let rec loop acc lines =
      match lines with
      | [] -> acc, []
      | "" :: rest -> acc, rest
      | line :: rest -> loop (line :: acc) rest

    let result, rest = loop List.empty lines

    result |> Seq.rev |> Grid.ofLines, rest

  Seq.unfold
    (function
    | [] -> None
    | lines -> Some(readGrid lines))
    (List.ofArray lines)
  |> List.ofSeq

let findReflections (items: seq<'a>) =
  let hashes = items |> Seq.map _.GetHashCode() |> Array.ofSeq

  let isMirror idx =
    let toCheck = min idx (hashes.Length - idx)

    seq { 0 .. toCheck - 1 }
    |> Seq.all (fun offset -> hashes[idx + offset] = hashes[idx - offset - 1])

  seq { 1 .. hashes.Length - 1 } |> Seq.filter isMirror

let columnReflections grid =
  grid |> Grid.columns |> findReflections |> Seq.map Col

let rowReflections grid =
  grid |> Grid.rows |> findReflections |> Seq.map Row

let findReflectionLines grid =
  Seq.append (columnReflections grid) (rowReflections grid)

let score =
  function
  | Col x -> x
  | Row x -> x * 100

let part1 () =
  input |> Seq.collect findReflectionLines |> Seq.map score |> Seq.sum

let reflectionWithSmudge g =
  let originalReflection = findReflectionLines g |> Seq.head

  seq {
    for rowIdx in seq { 0 .. Grid.rowsCount g - 1 } do
      for colIdx in seq { 0 .. Grid.columnsCount g - 1 } do
        yield
          Grid.updateWith
            { Row = rowIdx; Col = colIdx }
            (function
            | '.' -> '#'
            | _ -> '.')
            g
  }
  |> Seq.collect findReflectionLines
  |> Seq.find ((<>) originalReflection)


let part2 () =
  input |> Seq.map reflectionWithSmudge |> Seq.map score |> Seq.sum


printfn "Day 13"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
