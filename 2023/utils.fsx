module Utils

open System

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

  let tryAt idx (s: string) =
    if 0 <= idx && idx < s.Length then Some(s[idx]) else None

  let join (separator: string) (items: 'a seq) = String.Join(separator, items)

module List =
  let repeat n xs =
    xs |> List.replicate n |> List.collect id

module Array =
  let tryAt idx (xs: 'a array) =
    if 0 <= idx && idx < xs.Length then Some(xs[idx]) else None


type Grid<'a> = Grid of array<array<'a>>
type GridPos = { Row: int; Col: int }

module Grid =
  let create items = Grid items

  let ofLines (lines: string seq) =
    lines |> Seq.map _.ToCharArray() |> Array.ofSeq |> create

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

  let updateWith pos update (Grid content) =
    let newRow =
      Array.updateAt
        pos.Col
        (update <| content[pos.Row][pos.Col])
        content[pos.Row]

    content |> Array.updateAt pos.Row newRow |> create

  let positions grid =
    seq {
      for row in seq { 0 .. rowsCount grid - 1 } do
        for col in seq { 0 .. columnsCount grid - 1 } do
          yield { Col = col; Row = row }
    }

  let tryAt pos (Grid content) =
    content |> Array.tryAt pos.Row |> Option.bind (Array.tryAt pos.Col)

  let at pos grid = tryAt pos grid |> Option.get

  let pick pred grid =
    grid |> positions |> Seq.filter (fun pos -> at pos grid |> pred) |> set

  let toString fmt grid =
    rows grid
    |> Seq.mapi (fun rowIdx row ->
      row
      |> Seq.mapi (fun colIdx x -> fmt x { Col = colIdx; Row = rowIdx })
      |> String.join "")
    |> String.join Environment.NewLine

  let isWithin { Row = r; Col = c } (Grid content) =
    0 <= r && r < content.Length && 0 <= c && c < content[r].Length
