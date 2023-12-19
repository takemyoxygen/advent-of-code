module Utils

module Input =
  open System.IO

  let readLines (day: int) (test: bool) =
    let inputFile =
      Path.Combine(
        __SOURCE_DIRECTORY__,
        "input",
        sprintf "day%d%s.txt" day (if test then "-test" else "")
      )

    File.ReadAllLines inputFile

module Seq =
  let all pred xs = xs |> Seq.exists (pred >> not) |> not

module String =
  let tryGet idx (s: string) =
    if 0 <= idx && idx < s.Length then Some(s[idx]) else None

module List =
  let repeat n xs =
    xs |> List.replicate n |> List.collect id


type Grid<'a> = Grid of array<array<'a>>

module Grid =
  let create items = Grid items

  let column colIdx (Grid content) =
    seq { 0 .. content.Length - 1 }
    |> Seq.map (fun rowIdx -> content[rowIdx][colIdx])
    |> List.ofSeq

  let row rowIdx (Grid content) = content[rowIdx] |> List.ofArray

  let columnsCount (Grid content) = content[0].Length

  let rowsCount (Grid content) = content.Length

  let columns grid =
    seq { 0 .. (columnsCount grid) - 1 } |> Seq.map (fun idx -> column idx grid)

  let rows (Grid content) = content |> Seq.map (List.ofArray)

  let updateWith row column update (Grid content) =
    let newRow =
      Array.updateAt column (update <| content[row][column]) content[row]

    content |> Array.updateAt row newRow |> create
